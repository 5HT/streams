-module(bcd).
-compile(export_all).

decode(Bits) -> list_to_integer([X+$0 || <<X:4>> <= Bits]).
encode(N, S) when N >= 0, S > 0 -> << <<(X-$0):4>>
               || X <- tl(integer_to_list(trunc(math:pow(10,S*2)) + (N rem trunc(math:pow(10,S*2))))) >>.

