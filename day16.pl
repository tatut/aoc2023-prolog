:- use_module(grid).

in(G) :- grid_from_file('day16.txt', G).


% dir(Char, Dir, NewDir) to determine next travel direction(s)
dir(46, Dir, Dir). % .
dir(47, r, u). dir(47, l, d). dir(47, d, l). dir(47, u, r). % /
dir(92, r, d). dir(92, l, u). dir(92, u,  l). dir(92, d, r). % \
dir(45, r, r). dir(45, l, l). dir(45, u, l). dir(45, u, r). dir(45, d, l). dir(45, d, r). % -
dir(124, u, u). dir(124, d, d). dir(124, l, u). dir(124, l, d). dir(124, r, u). dir(124, r, d). % |

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
    empty_nb_set(S), empty_nb_set(V),
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
    in(G), aggregate_all(max(E), (edge_tile(G, X, Y, D), energized_count(G, X,Y,D, E)), Ans).
% part2(6701).
