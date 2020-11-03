:- use_module(tables).
:- use_module(check_predicates).
:- use_module(library(clpfd)).

plus5(X, Y) :- Y is X + 5.

make_format_str(MaxRowLen, Str) :-
				maplist(plus5, MaxRowLen, Rp), aux_format(Rp, Str).

aux_format([H], R) :- string_concat("~t~w~t~", H, R1),
					  string_concat(R1, "+~n", R), !.

aux_format([H|T], R) :- string_concat("~t~w~t~", H, R1),
						string_concat(R1, "+ ", R2),
						aux_format(T, Rp),
						string_concat(R2, Rp, R).

%predicat care scoate un vector care contine valorile maxime din 2 vectori
maxOnLine([E|L], [E2|L2], [E|R]) :- E > E2, maxOnLine(L, L2, R), !.
maxOnLine([E|L], [E2|L2], [E2|R]) :- E < E2, maxOnLine(L, L2, R), !.
%daca sunt egale elementele, pun unul dintre ele, nu conteaza care
maxOnLine([E|L], [E2|L2], [E2|R]) :- E = E2, maxOnLine(L, L2, R), !.
maxOnLine(L, [], L).
maxOnLine([], L, L).

%predicat care transforma o lista de stringuri in o lista de lungimi
fromStringToNumbers([], []).
fromStringToNumbers([E|T], [E2|R]) :- call(string_length, E, E2), fromStringToNumbers(T,R).

%predicatul map (implementarea de la curs)
mapp([], _, []).
mapp([E|T], Op, [E2|R]) :- call(Op, E, E2), mapp(T, Op, R).

%predicat care calculeaza vectorul cu lungimile maxime de pe coloane
get_max([E], E).
get_max([E|Mat], Res) :- get_max(Mat, R), maxOnLine(E, R, Res), !.


%predicat care returneaza direct tin tabel vectorul de lungimi maxime de pe coloane
getArray(L, R) :- mapp(L, fromStringToNumbers, Res), get_max(Res, R),!.

%predicat auxiliara pentru print_table_op
aux_print_table([E], V) :- make_format_str(V, Str), format(Str, E), !.
aux_print_table([E|L], V) :- make_format_str(V, Str), format(Str, E), aux_print_table(L, V), !.

%daca e o lista cu un singur element, apelez functiile pentru el
print_table_op([E]) :- getArray([E], MaxRowLen),  aux_print_table([E], MaxRowLen).
print_table_op([E|L]) :- getArray([E|L], MaxRowLen), aux_print_table([E|L], MaxRowLen).

%predicat care returneaza primul element dintr-o lista
head([], []).
head([E|_], E).

%predicat care filtreaza coloanele din tabelul transpus
filterLines([], _, []).
%daca primul element din lista se afla in lista de coloane, il adaug la rezultat
filterLines([E|L], Selected, [E|R]) :- head(E,H), member(H, Selected), filterLines(L, Selected, R), !.
filterLines([_|L], Selected, R) :- filterLines(L, Selected, R), !.

%predicat care preia rezultatul pe tabelul transpus
getTransposedSelection(Table, Selected, R) :- transpose(Table, TranspTable), filterLines(TranspTable, Selected, R).

%se transpune rezultatul transpus => tabelul normal
select_op(T, Cols, R) :- getTransposedSelection(T, Cols, Res), transpose(Res, R).

%predicat auxiliara care aplica Op pe fiecare 2 linii din tabele
joinAux(Op, [E], [E2], [Res|_]) :- call(Op, E, E2, Res).
joinAux(Op, [E|L], [E2|L2], [Res|R]) :- call(Op, E, E2, Res), joinAux(Op, L, L2, R).


join_op(Op, T1, T2, R) :- joinAux(Op, T1, T2, R).

%predicat care intoarce lista fara primul element
tail([_], []).
tail([_|L],L).

%aici folosesc smecheria de la curs cu not-ul
filter_op([], _, _, []).
filter_op([E|T], Vars, Pred, R) :- not((E = Vars, Pred)), filter_op(T, Vars, Pred, R), !.
filter_op([E|T], Vars, Pred, [E|R]) :- filter_op(T, Vars, Pred, R). 



%predicat care filtreaza un student dupa note
filterGrades(E) :- call(nth0, 2, E, Grade1),
				   call(nth0, 3, E, Grade2),
				   call(nth0, 4, E, Grade3),
				   call(nth0, 5, E, Grade4),
				   call(nth0, 6, E, Grade5),
				   X is (Grade1 + Grade2 + Grade3 + Grade4 + Grade5) / 5,
				   X > 5.
%predicat care filtreaza un student dupa nume si valorile in ascii
filterName(E) :- string_to_list(E,L),
				 reverse(L, Name), 
				 call(nth0, 0, Name, 117),
				 call(nth0, 1, Name, 99),
				 call(nth0, 2, Name, 115),
				 call(nth0, 3, Name, 101).

%predicat care filtreaza un student cu ajutorul celorlalte filtre
filterLine(E) :- call(nth0, 2, E, Grade1),  call(nth0, 3, E, Grade2), (Grade1 + Grade2) / 2 > 6, filterGrades(E),
				 call(nth0, 1, E, Name), filterName(Name).

%predicat auxiliar pentru complexQ
filterTable([], []).
filterTable([E], [E]) :- filterLine(E).
filterTable([E|T], [E|R]) :- filterLine(E), filterTable(T, R), !.
filterTable([_|T], R) :- filterTable(T, R).

%predicat folosit pentru query-ul complex_query1
complexQ(Table, [Head|R]) :- head(Table, Head), tail(Table, Tail), filterTable(Tail, R).

eval(table(Nume), Tabel) :- table_name(Nume, Tabel).
eval(tprint(Query), Tabel) :- eval(Query, Tabel), print_table_op(Tabel).
eval(select(Columns, Query), Tabel) :- eval(Query, Tabel1), select_op(Tabel1, Columns, Tabel).
eval(join(Op, NewCols, Tabel1, Tabel2), [NewCols|Tabel]) :- eval(Tabel1, T1), eval(Tabel2, T2), join_op(Op, T1, T2, TabelAux), tail(TabelAux, Tabel).
eval(tfilter(Vars, Pred, Query), [Head|Table]) :- eval(Query, Table1), head(Table1, Head), tail(Table1, Table2), filter_op(Table2, Vars, Pred, Table).
eval(complex_query1(Query), Table) :- eval(Query, Table1), complexQ(Table1, Table).