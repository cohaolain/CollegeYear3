path(A,C,G) :- 
    member([A,B],G),
    path(B,C,G).

path(A,B,G) :-
    member([A,B], G).