runs([]).
runs([A|T]) :- T = [B|C], A = B, runs(T).