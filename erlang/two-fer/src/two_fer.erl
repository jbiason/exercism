-module(two_fer).

-import(string, [concat/2]).

-export([two_fer/0, two_fer/1]).


two_fer() -> 
	"One for you, one for me.".

two_fer(Name) -> 
	concat(
	  concat("One for ", Name),
	  ", one for me."
	 ).
