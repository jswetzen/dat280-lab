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

% Distributed

map_reduce_dist(Map,M,Reduce,R,Input) ->
  Parent = self(),
  Nodes = nodes(),
  Nodes_Len = length(Nodes),
  Splits = split_into(Nodes_Len,Input),
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
  Node = index_of(Node_id,Nodes),
  rpc:call(Node,map_reduce,spawn_reducer,[Parent,Reduce,I,Mapped]).

index_of(Item, List) -> index_of(Item, List, 0).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

% end of distributed

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
