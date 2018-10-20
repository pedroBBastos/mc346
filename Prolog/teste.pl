read_lines([H|T]):- read_line_to_codes(user_input,H), H \= end_of_file, read_lines(T).
read_lines([]).

%main :-
%  read_stream_to_codes(user_input,X), writef("%s",[X]).

transf([H], R) :- string_codes(String, H), R = [String].
transf([H|T], R) :- string_codes(String, H), transf(T, RR), append([String], RR, R).

main :-
  read_lines(X),
  transf(X, R),
  print(R).
