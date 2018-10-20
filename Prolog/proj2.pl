% Pedro Barros Bastos      RA : 204481
% Rafael Ferreira Galib    RA : 204904

main :-
    entrada(X),
    transf(X, S),
    processa(S, R),
    saida(R).

% -----------------------------------------

% predicado para montar vetor com linhas da entrada
% entrada(-ListaDeListadeCodigosASCII)
entrada([H|T]):- read_line_to_codes(user_input,H), H \= end_of_file, entrada(T).
entrada([]).

% predicado para transformar vetor de codigos ascii para string
% transf(+ListaDeListadeCodigosASCII, -ListaDeStrings)
transf([H], R) :- string_codes(String, H), R = [String].
transf([H|T], R) :- string_codes(String, H), transf(T, RR), append([String], RR, R).

% predicado para montar a saida
% saida(+ListaDeStrings)
saida([H|T]) :- write("\n"), write(H), saida(T).
saida([]).

% -----------------------------------------

% predicado que executa as possiveis combinacoes enquanto for possivel faze-las
% processa(+ListaDeStrings, -ListaDeStrings)
processa(S, R) :-
  montaGenoma(S, RR),
  length(S, LI), length(RR, LF),
  (LI = LF -> R = RR; processa(RR, R)).

% predicado usado no foldl de montaGenoma
%p(+Genoma, +Acumulador, -NovoAcum)
p(G, [], [G]) :- !, true.
p(G, [H|T], NovoAcc) :-
    combine(H, G, R1),
    (R1 = "" ->  combine(G, H, R2),
        (R2 = "" ->  p(G, T, PP), NovoAcc = [H|PP];
         NovoAcc = [R2|T]
        );
     NovoAcc = [R1|T]).

% predicado que realiza uma possivel combinacao
%montaGenoma(+ListaDeStrings, -ListaDeStrings)
montaGenoma(L, R) :- foldl(p, L, [], R).

% -----------------------------------------
% predicados auxiliares

% predicado para adicionar item no inicio de uma lista
% prepend(+Lista, +Item, -NovaLista)
prepend(L, E, [E|L]).

% predicado para retornar item inicial de uma lista de strings
% head(+Lista, -Item)
head([], "").
head([H|_], H).

% predicado que, se o fim da primeira string e o inicio da segunda string forem compativeis, une as duas strings
% combine(+String1, +String2, -StringCombinacao)
combine(S1, S2, R) :-
    commonForMe(S1, S2, 1, [], V),
    head(V, H),
    string_length(H, LengthCommon),
    (  LengthCommon > 3
    -> sub_string(S2, LengthCommon, _, 0, S2WithoutCommon), string_concat(S1, S2WithoutCommon, R);
    R = "").

% predicado que retorna o que ha em comum entre o fim de uma string e o inicio de outra string
%commonForMe(+String1, +String2, +TamanhoComparacao, +Acc, -StringEmComum)
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
