%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-compile(export_all).

%% Use map_reduce to count word occurrences

map(Url,ok) ->
  [{Url,Body}] = dets:lookup(web,Url),
  Urls = crawl:find_urls(Url,Body),
  [{U,1} || U <-Urls].

reduce(Url,Ns) ->
  [{Url,lists:sum(Ns)}].

%% 188 seconds
page_rank() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
  map_reduce:map_reduce_seq(fun map/2, fun reduce/2,
                            [{Url,ok} || Url <-Urls]).

%% 86 seconds
page_rank_par() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
  map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,
                            [{Url,ok} || Url <-Urls]).

%% Distributed
page_rank_dist() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
  map_reduce:map_reduce_dist(fun map_dist/2, 32, fun reduce/2, 32,
                            [{Url,ok} || Url <-Urls]).
%% Distributed map
map_dist(Fun,Xs) ->
  dets:open_file(web,[{file,"web.dat"}]),
  map(Fun,Xs).

%% Workers Pool
page_rank_pool() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
  map_reduce:map_reduce_pool(fun map_dist/2, 32, fun reduce/2, 32,
                            [{Url,ok} || Url <-Urls]).
%% Fault-Tolerance
page_rank_ft() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
  map_reduce:map_reduce_ft(fun map_dist/2, 32, fun reduce/2, 32,
                            [{Url,ok} || Url <-Urls]).
