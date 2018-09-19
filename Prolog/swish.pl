% tamanho de uma lista
tam([], 0).
tam([_|R], N) :- tam(R, NN), N is NN+1.

% soma dos elementos de uma lista soma(+LISTA,-SOMA)
soma([], 0).
soma([E|R], S) :- soma(R, SS), S is SS+E.

% soma dos números pares de uma lista somap(+LISTA,-SOMA)
ehPar(V, E) :- 0 is mod(V,2), E is V. % 'is' eh usado quando 1o precisamos avaliar a expressao do lado direito
ehPar(_, E) :- E is 0.

somaPares([], 0).
somaPares([E|R], S) :- somaPares(R, SS), ehPar(E, V), S is SS+V.

% soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1) somapares(+LISTA,-SOMA)
somaPosicoesParesAux([], 0, 0).
somaPosicoesParesAux([E|R], T, S) :- TT is T-1, somaPosicoesParesAux(R, TT, SS), 

somaPosicoesPares([E|R], S) :- 