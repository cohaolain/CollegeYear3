occurs(_,[],0).
occurs(E,[E|L],N) :- occurs(E, L, M), N is M + 1.
occurs(E,[Y|L],N) :- not(E=Y), occurs(E,L,N).

thrice(E, L) :- occurs(E, L, 3).