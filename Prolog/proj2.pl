aux(S, Acc, NovoAcc) :-
    combine(Acc, S, R1),
    ( R1 = ""
      ->  combine(S, Acc, R2),
          (R2 = "" -> NovoAcc = Acc; NovoAcc = R2);
      NovoAcc = R1).

% resulta na melhor juncao de uma string S com alguma das strings do vetor V
melhorJuncao(S, V, SR) :- foldl(aux, V, S, SR).

% depois ir chamando sub_atom de acordo com o retorno do melhorJuncao

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

% predicado para montar vetor com linhas da entrada
entradas(S) :-
    read(X),
    (   X = end_of_file -> S = []; % verificar quando usar '' ou ""
    	  entradas(SS),
        append([X], SS, S)
    ).

main :-
    entradas(S),
    print(S).
