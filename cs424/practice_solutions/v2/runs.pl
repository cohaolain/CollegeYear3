runs([]).
runs([H|[HT|TT]]) :-
    H = HT,
    runs([HT|TT]);
    H = HT,
    runs(TT).