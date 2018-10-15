prepend(L, E, [E|L]).

head([], "").
head([H|_], H).

% commonForMe("abcfcf", "cfcfdd", 1, [], X)
commonForMe(S1, S2, L, Acc, R) :-
    string_length(S1, L1), string_length(S2, L2),
    (L > L1 ->  R = Acc;
     L > L2 ->  R = Acc;
    	sub_string(S1, _, L, 0, LastCharS1),
    	sub_string(S2, 0, L, _, LastCharS2),
        LL is L+1,
    	(LastCharS1 = LastCharS2
    	->  prepend(Acc, LastCharS1, Acc2),
            commonForMe(S1, S2, LL, Acc2, R);
        commonForMe(S1, S2, LL, Acc, R))).

combine(S1, S2, R) :-
    commonForMe(S1, S2, 1, [], V),
    head(V, H),
    string_length(H, LengthCommon),
    (  LengthCommon > 3
    -> sub_string(S2, LengthCommon, _, 0, S2WithoutCommon), string_concat(S1, S2WithoutCommon, R);
    R = "").

% -----------------------------------------

% P( +Genoma, +Acumulador, -NovoAcum)
p(G, [], [G]).
p(G, [H|T], NovoAcc) :-
    combine(H, G, R1),
    (R1 = "" ->  combine(G, H, R2),
        (R2 = "" ->  p(G, T, PP), NovoAcc = [H|PP];
         NovoAcc = [R2|T]
        );
     NovoAcc = [R1|T]).

montaGenoma(L, R) :- foldl(p, L, [], R).

% montaGenoma(["xxxxxababababyyyyyy", "yyaaaaaaaaaaa", "yyyyyyeeeeeeeeeeeeee", "cccccccccccccccxxxxx", "fffffffffffffffwwwwww", "wwwwwwgggggggggggxx"], X).
