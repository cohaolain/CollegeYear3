mul(A, B, O) :-
    length(O, Lo),
    length(A, La),
    length(B, Lb),
    Lo =:= La * Lb,
    !.