scissors(L, I, A, B) :-
    append(A, [I], S),
    append(S, B, L).
