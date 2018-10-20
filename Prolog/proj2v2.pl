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

%p( +Genoma, +Acumulador, -NovoAcum)
p(G, [], [G]) :- !, true.
p(G, [H|T], NovoAcc) :-
    combine(H, G, R1),
    (R1 = "" ->  combine(G, H, R2),
        (R2 = "" ->  p(G, T, PP), NovoAcc = [H|PP];
         NovoAcc = [R2|T]
        );
     NovoAcc = [R1|T]).

montaGenoma(L, R) :- foldl(p, L, [], R).
%montaGenoma(["xxxxxababababyyyyyy", "yyaaaaaaaaaaa", "yyyyyyeeeeeeeeeeeeee", "cccccccccccccccxxxxx", "fffffffffffffffwwwwww", "wwwwwwgggggggggggxx"], X).

% -----------------------------------------

% predicado para montar vetor com linhas da entrada
entrada([H|T]):- read_line_to_codes(user_input,H), H \= end_of_file, entrada(T).
entrada([]).

transf([H], R) :- string_codes(String, H), R = [String].
transf([H|T], R) :- string_codes(String, H), transf(T, RR), append([String], RR, R).

saida([H|T]) :- write("\n"), write(H), saida(T).
saida([]).

main :-
    entrada(X),
    transf(X, S),
    montaGenoma(S, R),
    saida(R).
