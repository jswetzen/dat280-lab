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
  Inputs = [KV


            || Mapped <-Mappeds,
               {J,KVs} <-Mapped,
               I==J,
               KV <-KVs],


  spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)}
             end).
