:- module(grid, [grid_from_file/2, grid_from_string/2, at/4, set_at/4, size/3, show/1, pos/4, move/5]).

grid_from_string(String, g(W,H,Data)) :-
    string_lines(String, Lines),
    length(Lines, H),
    [L|_]=Lines,
    string_length(L, W),
    maplist(string_codes, Lines, LineCodes),
    append(LineCodes, Codes1),
    compound_name_arguments(Data, data, Codes1).

grid_from_file(File, Grid) :- read_file_to_string(File, Str, []), grid_from_string(Str, Grid).

size(g(W,H,_), W, H).
pos(g(W,H,_), X, Y, Pos) :- between(1,W,X), between(1,H,Y), Pos is (Y-1) * W + X.
at(g(_,_,Data), Pos, C) :- arg(Pos, Data, C).
at(G, X, Y, C) :- pos(G, X, Y, Pos), at(G, Pos, C).
set_at(g(_,_,Data), Pos, C) :- nb_setarg(Pos, Data, C).
set_at(G, X, Y, C) :- pos(G, X, Y, Pos), set_at(G, Pos, C).

repeat(0, _, []).
repeat(N, X, [X|Rest]) :- N > 0, succ(N1, N), repeat(N1, X, Rest).

% print grid to display
show(G) :-
    size(G, W, H),
    repeat(W, `-`, Sep),
    append([[43],Sep,[43]], SepCs),
    string_codes(SepStr, SepCs),
    writeln(SepStr),
    numlist(1,H, Ys),
    maplist({G,W}/[Y]>>( numlist(1, W, Xs),
                         maplist({G,Y}/[X,Ch]>>at(G,X,Y,Ch), Xs, Cs),
                         append([[124],Cs,[124]], Cs1),
                         string_codes(Str, Cs1),
                         writeln(Str) ), Ys),
    writeln(SepStr), writeln("").

% move to get next position in given direction
move(X,Y, r, X1,Y) :- succ(X,X1).
move(X,Y, l, X1,Y) :- succ(X1,X).
move(X,Y, u, X,Y1) :- succ(Y1, Y).
move(X,Y, d, X,Y1) :- succ(Y, Y1).

move(X,Y, e, X1,Y) :- succ(X,X1).
move(X,Y, w, X1,Y) :- succ(X1,X).
move(X,Y, n, X,Y1) :- succ(Y1, Y).
move(X,Y, s, X,Y1) :- succ(Y, Y1).
