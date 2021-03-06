%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% This is a very simple implementation of map-reduce, in both
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


-module(map_reduce).
-compile(export_all).


%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce.


%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.


map_reduce_seq(Map,Reduce,Input) ->
  Mapped = [{K2,V2}
            || {K,V} <-Input,
               {K2,V2} <-Map(K,V)],
  reduce_seq(Reduce,Mapped).


reduce_seq(Reduce,KVs) ->
  [KV || {K,Vs} <-group(lists:sort(KVs)),
         KV <-Reduce(K,Vs)].


group([]) ->
  [];
group([{K,V}|Rest]) ->
  group(K,[V],Rest).


group(K,Vs,[{K,V}|Rest]) ->
  group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
  [{K,lists:reverse(Vs)}|group(Rest)].

%%%%%%%%%%%%%%%%%%%
%%% Distributed %%%
%%%%%%%%%%%%%%%%%%%

map_reduce_dist(Map,M,Reduce,R,Input) ->
  Parent = self(),
  Nodes = [node()|nodes()],
  Splits = split_into(length(Nodes),Input),
  Split_Nodes = lists:zip(Splits, Nodes),
  Mappers =
  lists:flatten([remote_mapper(Parent,M,Map,R,Split,Node)
   || {Split, Node} <-Split_Nodes]),
  Mappeds =
  [receive {Pid,L} -> L end || Pid <-Mappers],
  Reducers =
  lists:flatten([remote_reducer(Parent,Reduce,I,Mappeds,Nodes)
   || I <-lists:seq(0,R-1)]),
  Reduceds =
  [receive {Pid,L} -> L end || Pid <-Reducers],
  lists:sort(lists:flatten(Reduceds)).

remote_mapper(Parent,M,Map,R,Split,Node) ->
  [rpc:call(Node,map_reduce,spawn_mapper,[Parent,Map,R,S])
   || S <- split_into(M,Split)].

remote_reducer(Parent,Reduce,I,Mapped,Nodes) ->
  Node_id = I rem length(Nodes),
  Node = lists:nth(Node_id+1,Nodes),
  rpc:call(Node,map_reduce,spawn_reducer,[Parent,Reduce,I,Mapped]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Load-balancing Map-Reduce %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_reduce_pool(Map,M,Reduce,R,Input) ->
  Splits = split_into(M,Input),
  Mappeds = worker_pool([fun() ->
                             Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                                       || {K,V} <- Split,
                                          {K2,V2} <-Map(K,V)],
                             group(lists:sort(Mapped))
                         end || Split <- Splits]),
  Reducers = lists:flatten(
               worker_pool(
                 [fun() ->
                      In = [KV || Mapped <-Mappeds,
                                  {J,KVs} <-Mapped,
                                  I==J,
                                  KV <-KVs],
                      reduce_seq(Reduce,In)
                  end || I <-lists:seq(0,R-1)]
                )),
  lists:sort(Reducers).

worker_pool(Funs) ->
  Nodes = nodes(),
  Pids = [spawn_link(Node,map_reduce,work,[self()]) || Node <- Nodes],
  Results = pool(Funs,[],0),
  [Pid ! {die} || Pid <- Pids],
  [receive {available,_} -> ok end || _ <- Nodes],
  Results.

pool([],Results,0) -> Results;

pool([],Results,Wait_For) ->
  receive {work_done,Result} -> pool([],[Result|Results],Wait_For-1) end;

pool([Fun|Funs],Results,Wait_For) ->
  receive {available,Node} -> Node ! {work,Fun},
                              pool(Funs,Results,Wait_For+1);
          {work_done,Result} -> pool([Fun|Funs],[Result|Results],Wait_For-1)
  end.

work(Pool) ->
  Pool ! {available,self()},
  receive {work,Fun} -> Result = Fun(),
                        Pool ! {work_done,Result},
                        work(Pool);
          {die} -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fault-tolerant Map-Reduce %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_reduce_ft(Map,M,Reduce,R,Input) ->
  Splits = split_into(M,Input),
  Mappeds = worker_pool_ft([fun() ->
                             Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                                       || {K,V} <- Split,
                                          {K2,V2} <-Map(K,V)],
                             group(lists:sort(Mapped))
                         end || Split <- Splits]),
  Reducers = lists:flatten(
               worker_pool_ft(
                 [fun() ->
                      In = [KV || Mapped <-Mappeds,
                                  {J,KVs} <-Mapped,
                                  I==J,
                                  KV <-KVs],
                      reduce_seq(Reduce,In)
                  end || I <-lists:seq(0,R-1)]
                )),
  lists:sort(Reducers).

worker_pool_ft(Funs) ->
  Nodes = nodes(),
  process_flag(trap_exit,true),
  Pids = [spawn_link(Node,map_reduce,work_ft,[self()]) || Node <- Nodes],
  Results = pool_ft(Funs,[],[],0),
  [Pid ! {die} || Pid <- Pids],
  [receive {available,_} -> ok end || _ <- Nodes],
  Results.

pool_ft([],Results,_,0) -> Results;

pool_ft([],Results,Workers,Wait_For) ->
  receive
    {work_done,Result,Node} ->
      {_,_,New_Workers} = lists:keytake(Node,1,Workers),
      pool_ft([],[Result|Results],New_Workers,Wait_For-1);
    {'EXIT',Pid,_} ->
      {_,{_,Fun},New_Workers} = lists:keytake(Pid,1,Workers),
      pool_ft([Fun],Results,New_Workers,Wait_For-1)
  end;

pool_ft([Fun|Funs],Results,Workers,Wait_For) ->
  receive {available,Node} ->
            Node ! {work,Fun},
            New_Workers = [{Node,Fun}|Workers],
            pool_ft(Funs,Results,New_Workers,Wait_For+1);
          {work_done,Result,Node} ->
            {_,_,New_Workers} = lists:keytake(Node,1,Workers),
            pool_ft([Fun|Funs],[Result|Results],New_Workers,Wait_For-1);
          {'EXIT',Pid,_} -> case lists:keytake(Pid,1,Workers) of
                              {_,{_,Fun2},New_Workers} ->
                                pool_ft([Fun2|[Fun|Funs]],Results,New_Workers,Wait_For-1);
                              false -> pool_ft([Fun|Funs],Results,Workers,Wait_For)
                            end
  end.

work_ft(Pool) ->
  Pool ! {available,self()},
  receive {work,Fun} -> Result = Fun(),
                        Pool ! {work_done,Result,self()},
                        work_ft(Pool);
          {die} -> ok
  end.

% End of FT MR

map_reduce_par(Map,M,Reduce,R,Input) ->
  Parent = self(),
  Splits = split_into(M,Input),
  Mappers =
  [spawn_mapper(Parent,Map,R,Split)
   || Split <-Splits],
  Mappeds =
  [receive {Pid,L} -> L end || Pid <-Mappers],
  Reducers =
  [spawn_reducer(Parent,Reduce,I,Mappeds)
   || I <-lists:seq(0,R-1)],
  Reduceds =
  [receive {Pid,L} -> L end || Pid <-Reducers],
  lists:sort(lists:flatten(Reduceds)).


spawn_mapper(Parent,Map,R,Split) ->
  spawn_link(fun() ->
                 Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                           || {K,V} <-Split,
                              {K2,V2} <-Map(K,V)],
                 Parent !
                 {self(),group(lists:sort(Mapped))}
             end).


split_into(N,L) ->
  split_into(N,L,length(L)).

split_into(1,L,_) ->
  [L];
split_into(N,L,Len) ->
  {Pre,Suf} = lists:split(Len div N,L),
  [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
  Inputs = [KV || Mapped <-Mappeds,
               {J,KVs} <-Mapped,
               I==J,
               KV <-KVs],
  spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)}
             end).
