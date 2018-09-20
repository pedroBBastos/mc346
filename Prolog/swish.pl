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

% a lista ja esta ordenada?

head([H|_], H).
verificaMenor(A, B) :- A < B.

verificaOrdenacao([]).
verificaOrdenacao([_]).
verificaOrdenacao([H|T]) :- verificaOrdenacao(T), head(T, H2), verificaMenor(H, H2).

% dado n gera a lista de 1 a n

gera1aN(N, L) :- geraNa1(N, LL), reverteLista(LL, L).
geraNa1(0, []).
geraNa1(N, L) :- NN is N-1, geraNa1(NN, LL), L = [N|LL].

outroGera1aN(1, [1]).
outroGera1aN(N, L) :- NN is N-1,  outroGera1aN(NN, LL), concatena(LL, [N], LLL), L = LLL.

% retorna o ultimo elemento de uma lista

ultimoElemento([X], X).
ultimoElemento([_|T], E) :- ultimoElemento(T, EE), E = EE.

% retorna a lista sem o utlimo elemento

tail([_|T], T).

semUltimo([_], []).
semUltimo([H|T], E) :- semUltimo(T, EE), concatena([H], EE, E).

% shift right
% shiftr([1,2,3,4],X)
% X = [4,1,2,3]

shiftRight([], []).
shiftRight([X], [X]).
shiftRight(L, R) :- ultimoElemento(L, U), semUltimo(L, SU), concatena([U], SU, RR), R = RR.

% shiftr n lista (shift right n vezes)

shiftRightN([], _, []).
shiftRightN([X], _, [X]).
shiftRightN(L, 0, L).
shiftRightN(L, N, R) :- NN is N-1, shiftRight(L, RR), shiftRightN(RR, NN, R).

% shift left
% shiftl([1,2,3,4],X)
% X = [2,3,4,1]

shiftLeft([], []).
shiftLeft([X], [X]).
shiftLeft([H|T], R) :- concatena(T, [H], R).

% shift left n vezes

shiftLeftN([], _, []).
shiftLeftN([X], _, [X]).
shiftLeftN(L, 0, L).
shiftLeftN(L, N, R) :- NN is N-1, shiftLeft(L, RR), shiftLeftN(RR, NN, R).

% remove item da lista (1 vez so)
removeItem([], _, []).
removeItem([I|T], I, T).
removeItem([H|T], I, R) :- removeItem(T, I, RR), concatena([H], RR, R).

% remove item da lista (todas as vezes)
removeItem2([], _, []).
removeItem2([I|T], I, R) :- removeItem2(T, I, R).
removeItem2([H|T], I, R) :- removeItem2(T, I, RR), concatena([H], RR, R).

% remove item da lista n (as primeiras n vezes)
removeItem3([], _, _, []).
removeItem3(L, _, 0, L).
removeItem3([I|T], I, N, R) :- NN is N-1, removeItem3(T, I, NN, R).
removeItem3([H|T], I, N, R) :- removeItem3(T, I, N, RR), concatena([H], RR, R).

% remove item da lista (a ultima vez que ele aparece) **
removeItem4([], _, []).
removeItem4(L, I, R) :- reverteLista(L, LL), removeItem3(LL, I, 1, LLL), reverteLista(LLL, R).

% troca velho por novo na lista (1 so vez)
trocaVelhoPorNovo([], _, _, []).
trocaVelhoPorNovo([V|T], V, N, [N|T]).
trocaVelhoPorNovo([H|T], V, N, R) :- trocaVelhoPorNovo(T, V, N, RR), concatena([H], RR, R).

% troca velho por novo na lista (todas vezes)
trocaVelhoPorNovo2([], _, _, []).
trocaVelhoPorNovo2([V|T], V, N, R) :- trocaVelhoPorNovo2(T, V, N, RR), concatena([N], RR, R).
trocaVelhoPorNovo2([H|T], V, N, R) :- trocaVelhoPorNovo2(T, V, N, RR), concatena([H], RR, R).

% troca velho por novo na lista n (as primeiras n vezes)
trocaVelhoPorNovo3([], _, _, _, []).
trocaVelhoPorNovo3(L, _, _, 0, L).
trocaVelhoPorNovo3([V|T], V, N, Q, R) :- QQ is Q-1, trocaVelhoPorNovo3(T, V, N, QQ, RR), concatena([N], RR, R).
trocaVelhoPorNovo3([H|T], V, N, Q, R) :- trocaVelhoPorNovo3(T, V, N, Q, RR), concatena([H], RR, R).
