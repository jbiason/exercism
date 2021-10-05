-module(collatz_conjecture).

-export([steps/1]).


steps(1) -> 0;
steps(N) when N =< 0 -> error(badarg);
steps(N) when (N rem 2 == 0) -> 1 + steps(N div 2);
steps(N) -> 1 + steps(3*N+1).
