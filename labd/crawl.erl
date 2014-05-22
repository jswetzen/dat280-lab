%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module defines a simple web crawler using map-reduce.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(crawl).
-compile(export_all).

%% Crawl from a URL, following links to depth D.
%% Before calling this function, the inets service must
%% be started using inets:start().
crawl(Url,D) ->
  Pages = follow(D,[{Url,undefined}]),
  Results = [{U,Body} || {U,Body} <-Pages,
               Body /= undefined],
  dets:open_file(web,[{file, "web.dat"}, {type, set}]),
  dets:insert(web,Results),
  dets:close(web).

follow(0,KVs) ->
  KVs;
follow(D,KVs) ->
  follow(D-1,
         map_reduce:map_reduce_par(fun map/2, 20, fun reduce/2,
                                   1, KVs)).

map(Url,undefined) ->
  Body = fetch_url(Url),
  [{Url,Body}] ++
    lists:sublist([{U,undefined} || U <-find_urls(Url,Body)], 10);
map(Url,Body) ->
  [{Url,Body}].

reduce(Url,Bodies) ->
  case [B || B <-Bodies, B/=undefined] of
    [] ->
      [{Url,undefined}];
    [Body] ->
      [{Url,Body}]
  end.

fetch_url(Url) ->
  case httpc:request(Url) of
    {ok,{_,_Headers,Body}} ->
      Body;
    _ -> ""
  end.

%% Find all the urls in an Html page with a given Url.
find_urls(Url,Html) ->
  Lower = string:to_lower(Html),
  %% Find all the complete URLs that occur anywhere in the page
  Absolute = case re:run(Lower,"http://.*?(?=\")",[global]) of
               {match,Locs} ->
                 [lists:sublist(Html,Pos+1,Len)
                  || [{Pos,Len}] <-Locs];
               _ -> []
             end,
  %% Find links to files in the same directory, which need to be
  %% turned into complete URLs.
  Relative = case re:run(Lower,"href *= *\"(?!http:).*?(?=\")",[global]) of
               {match,RLocs} ->
                 [lists:sublist(Html,Pos+1,Len)
                  || [{Pos,Len}] <-RLocs];
               _ -> []
             end,
  Absolute ++ [Url++"/"++
               lists:dropwhile(
                 fun(Char)->Char==$/ end,
                 tl(lists:dropwhile(fun(Char)->Char/=$"
                                    end, R)))
               || R <-Relative].

