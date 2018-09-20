% tamanho de uma lista
tam([], 0).
tam([_|R], N) :- tam(R, NN), N is NN+1.

% soma dos elementos de uma lista soma(+LISTA,-SOMA)
soma([], 0).
soma([E|R], S) :- soma(R, SS), S is SS+E.

% soma dos números pares de uma lista somap(+LISTA,-SOMA)
ehPar(N) :- 0 is N mod 2.

somaPares([], 0).
somaPares([H|T], S) :-
    (ehPar(H), somaPares(T,SS), S is SS+H;
     somaPares(T,S)).

% soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1) somapares(+LISTA,-SOMA)
somaPosicaoPar([],0).
somaPosicaoPar([_], 0).
somaPosicaoPar([_, B|T], S) :- somaPosicaoPar(T, SS), S is SS+B.

% existe item na lista elem(+IT,+LISTA)
existe(X, [X|_]).
existe(X, [_|T]) :-
    existe(X, T).

% posição do item na lista: 1 se é o primeiro, falha se nao esta na lista pos(+IT,+LISTA,-POS)
posicao(I, [I|_], 1).
posicao(I, [_|T], P) :- posicao(I,T,PP), P is PP+1.

% conta quantas vezes o item aparece na lista (0 se nenhuma) conta(+IT,+LISTA,-CONTA)
contaAparicoes(_, [], 0).
contaAparicoes(I, [I|T], A) :- contaAparicoes(I, T, AA), A is AA+1.
contaAparicoes(I, [_|T], A) :- contaAparicoes(I, T, A).

% maior elemento de uma lista - maior(+LISTA,-MAX)
maior([X], X).
maior([H|T], M) :- maior(T, MM), (MM > H -> M is MM ; M is H).

verificaMaior(A, B) :- A > B.
maior2([X], X).
% .... ainda assim, precisaria de algo como o "if" do somaPares

% reverte uma lista
reverteLista([], []).
reverteLista(L, RL) :- reverteListaAux(L, RL, []).

reverteListaAux([], RL, RL).
reverteListaAux([H|T], RL, Acc) :- reverteListaAux(T, RLL, [H|Acc]), RL = RLL.


% % intercala1([1,2,3], [4,5,6,7,8], X).
%  X =  [1,4,2,5,3,6]

insere([], I, [I]).
insere([A], I, [A|[I]]).
insere([H|T], I, LR) :- insere(T, I, LLR), LR = [H|LLR].

intercala(L1, L2, LR) :- intercala1(L1, L2, LR, []).
intercala1(_, [], LR, LR).
intercala1([], _, LR, LR).
intercala1([H1|T1], [H2|T2], LR, Acc) :-
    insere(Acc, H1, Acc1),
    insere(Acc1, H2, Acc2),
    intercala1(T1, T2, LRR, Acc2), LR = LRR.

% intercala2([1,2,3], [4,5,6,7,8], Y),
% Y =   [1,4,2,5,3,6,7,8]

concatena(L1, [], L1).
concatena(L1, [X], LR) :- insere(L1, X, LLR), LR = LLR.
concatena(L1, [H|T], LR) :- insere(L1, H, LLR), concatena(LLR, T, LLLR), LR = LLLR.

intercalaSegundo(L1, L2, LR) :- intercala2(L1, L2, LR, []).
intercala2(L, [], LR, Acc) :- concatena(Acc, L, AccFinal), LR = AccFinal.
intercala2([], L, LR, Acc) :- concatena(Acc, L, AccFinal), LR = AccFinal.
intercala2([H1|T1], [H2|T2], LR, Acc) :-
    insere(Acc, H1, Acc1),
    insere(Acc1, H2, Acc2),
    intercala2(T1, T2, LRR, Acc2), LR = LRR.
