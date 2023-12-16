:- use_module(util).

parse(Codes, g(W,H,Data)) :-
    util:split_by(10, Codes, Lines),
    length(Lines, H),
    [L|_]=Lines,
    length(L, W),
    append(Lines, Codes1),
    compound_name_arguments(Data, data, Codes1).

in(G) :- read_file_to_codes('day16.txt', Cs, []), parse(Cs, G).

size(g(W,H,_), W, H).
pos(g(W,H,_), X, Y, Pos) :- between(1,W,X), between(1,H,Y), Pos is (Y-1) * W + X.
at(g(_,_,Data), Pos, C) :- arg(Pos, Data, C).
at(G, X, Y, C) :- pos(G, X, Y, Pos), at(G, Pos, C).

move(X,Y, r, X1,Y) :- succ(X,X1).
move(X,Y, l, X1,Y) :- succ(X1,X).
move(X,Y, u, X,Y1) :- succ(Y1, Y).
move(X,Y, d, X,Y1) :- succ(Y, Y1).

% dir(Char, Traveling, NewTraveling) to determine
% next travel direction
dir(46, Dir, Dir). % travel through empty
% /
dir(47, r, u).
dir(47, l, d).
dir(47, d, l).
dir(47, u, r).
% \
dir(92, r, d).
dir(92, l, u).
dir(92, u,  l).
dir(92, d, r).
% -
dir(45, r, r).
dir(45, l, l).
dir(45, u, l). dir(45, u, r).
dir(45, d, l). dir(45, d, r).
% |
dir(124, u, u).
dir(124, d, d).
dir(124, l, u). dir(124, l, d).
dir(124, r, u). dir(124, r, d).

% Travel beam at position in direction

travel(G, X,Y, _, _, _) :- \+ pos(G, X, Y, _). % outside mirror
travel(G, X,Y, Dir, Visited, Energize) :-
    at(G, X, Y, Here),
    add_nb_set(v(X,Y,Dir), Visited, true),
    call(Energize, X,Y), % Energize this location
    dir(Here, Dir, Dir1),
    move(X,Y, Dir1, X1,Y1),
    travel(G, X1, Y1, Dir1, Visited, Energize).

energize(S, X, Y) :- add_nb_set(X-Y, S).

energized_count(G, X, Y, Dir, Count) :-
    empty_nb_set(S),
    empty_nb_set(V),
    aggregate(max(Size), ( travel(G, X,Y,Dir, V, energize(S)), size_nb_set(S, Size) ), Count).

part1(Ans) :- in(G), energized_count(G, 1, 1, r, Ans).

% part1(6361).

% Part 2 get the maximum energized count when starting beam from any edge tile

% Determine edge tiles and directions
edge_tile(g(W,_,_), X, 1, d) :- between(1,W,X). % down from top edge
edge_tile(g(W,H,_), X, H, u) :- between(1,W,X). % up from bottom edge
edge_tile(g(_,H,_), 1, Y, r) :- between(1,H,Y). % right from left edge
edge_tile(g(W,H,_), W, Y, l) :- between(1,H,Y). % left from right edge

part2(Ans) :-
    in(G),
    aggregate_all(max(E), (edge_tile(G, X, Y, D), energized_count(G, X,Y,D, E)), Ans).
% part2(6701).
