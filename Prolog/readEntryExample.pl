main :-
    repeat,
    read(Term),
    (   Term = end_of_file
    ->  !
    ;   do_something_with(Term),
        fail
    ).

do_something_with(X) :- print(X).

constroiArray(I, [], [I]) :- !, true.
constroiArray(I, [H|T], R) :- constroiArray(I, T, RR), R = [H|RR].

% append([1], [3], X). ->>> isso produz [1,3]

% -> predicado abaixo cria um vetor de string a partir de cada linha da entrada
entradas(S) :-
    read(X),
    (   X = 'abc' -> S = [], !; % verificar quando usar '' ou ""
    	entradas(SS),
        append([X], SS, S)
    ).

% para o projeto, teriamos que fazer alguma iteracao O(n^l), sendo l o
% numero de linhas recebidas da entrada e verificar as melhores possibilidades
% de join.

% uso do substring -> http://www.swi-prolog.org/pldoc/man?predicate=sub_string/5
% sub_string(+String, ?Before, ?Length, ?After, ?SubString)
sub_string("minha rua eh dahorinha", 2, 3, X, L).

% tentar primeiro como determinar como colar duas strings

% para pegar ultimo caracter de string
sub_string("abdef", _, 1, 0, C).

% http://www.swi-prolog.org/pldoc/man?predicate=string_length/2

prepend(L, E, [E|L]).

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

head([], "").
head([H|_], H).

combine(S1, S2, R) :-
    commonForMe(S1, S2, 1, [], V),
    head(V, H),
    string_length(H, LengthCommon),
    (  LengthCommon > 0
    -> sub_string(S2, LengthCommon, _, 0, S2WithoutCommon), string_concat(S1, S2WithoutCommon, R);
    R = "").
