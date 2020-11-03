% Predicates made for testing

:- module(check_predicates, [grades_and_ratings/3, movies_and_ratings/3, check_names/3, names_and_ratings_pred/3]).

grades_and_ratings([_,_|Grds], [_,_|Rtngs], R) :- append(Grds, Rtngs, R).

movies_and_ratings([ID1,Title,Genre], [Fn,Sn,ID2,Rating], R) :-
                        ID1 = ID2,
                        append([Fn,Sn], [Title,Genre,Rating], R), !.
movies_and_ratings(_,_,["---", "---", "---", "---", "---"]).

check_names([FN,LN|T1], [FN,LN|T2], R) :- append(T1, T2, R), !.
check_names(_, _, ["---", "---", "---", "---", "---", "---"]).

names_and_ratings_pred(FN,_,RT) :- string_length(FN, L), L > 5, RT > 4, !.
names_and_ratings_pred(_,LN,RT) :- string_length(LN, L), L > 6, RT < 3.
