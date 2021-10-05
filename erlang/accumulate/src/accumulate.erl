-module(accumulate).

-export([accumulate/2]).

accumulate(Fn, [H|T]) ->
  [Fn(H) | accumulate(Fn, T)];
accumulate(_Fn, []) ->
  [].
