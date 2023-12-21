:- use_module(grid).

ex("...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........").

in(G) :- grid_from_file('day21.txt', G).

type(46, garden). % . is garden
type(83, garden). % S (start pos) is also garden
type(35, rock).   % # is rock
type(83, start).  % S

neighbor_pos(X1, Y1, X2, Y1) :- succ(X1,X2).
neighbor_pos(X1, Y1, X2, Y1) :- succ(X2,X1).
neighbor_pos(X1, Y1, X1, Y2) :- succ(Y1,Y2).
neighbor_pos(X1, Y1, X1, Y2) :- succ(Y2,Y1).

neighbor(G, X1, Y1, X2, Y2) :- neighbor_pos(X1,Y1,X2,Y2), at(G, X2,Y2, T), type(T, garden).

:- set_prolog_flag(stack_limit, 4_294_967_296).
:- set_prolog_flag(table_space, 4_294_967_296).
:- table reachable_in/4.


reachable_in(0, _, At, At). % reachable!
reachable_in(N, G, X-Y, Xr-Yr) :- N > 0,
                                  neighbor(G, X, Y, X1,Y1),
                                  succ(N1, N),
                                  reachable_in(N1, G, X1-Y1, Xr-Yr).

start_pos(G, X-Y) :-
    size(G, W,H), between(1,W,X), between(1,H,Y), at(G,X,Y,T), type(T,start).

% horribly inefficient way that uses lots of memory, but works for part1!
part1(Ans) :-
    in(G),
    start_pos(G,Start),
    aggregate_all(count, At, reachable_in(64, G, Start, At), Ans).
% part1(3615).


%% Part 2 take 26501365 steps
% idea1: calculate circle area (reachable with manhattan distance)
% remove all the rocks in the garden times how many gardens fit
% within the circle
%
% how about garden copies partially within the circle?
