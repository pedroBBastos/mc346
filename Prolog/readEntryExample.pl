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

% predicado que retorna o que ha em comum entre o fim de S1 e o inicio de S2
commonForMe(S1, S2, R, L) :-
    sub_string(S1, _, 1, 0, LastCharS1),
    sub_string(S2, 0, 1, _, LastCharS2),
    (LastCharS1 = LastCharS2
    ->  sub_string(S1, 0, _, 1, WithoutLastChar1),
        sub_string(S2, 1, _, 0, WithoutLastChar2),
        commonForMe(WithoutLastChar1, WithoutLastChar2, RR, LL),
        string_concat(RR, LastCharS1, R),
        L is LL + 1;
    R = "", L = 0).

% predicado para combinar duas strings que possuem final e início, respectivamente, em comum
combine(S1, S2, R) :-
    commonForMe(S1, S2, _, LengthCommon),
    (  LengthCommon > 0
    -> sub_string(S2, LengthCommon, _, 0, S2WithoutCommon), string_concat(S1, S2WithoutCommon, R);
    R = "").
