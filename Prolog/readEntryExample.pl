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
%
% Pensar numa maneira boa de comparar inicio/fim das strings
