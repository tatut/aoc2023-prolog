:- use_module(grid).
:- use_module(library(heaps)).

in(G) :- grid:grid_from_file('day17.txt', G).

% State must be s(X, Y, Dir, Count) where Dir is the direction
% we are facing and Count is the number of times we have moved
% in this direction.
%
% From that we can calculate neighbors and use A*
% neighboring

% Can move forward
neighbor(G, s(X, Y, Dir, Count), s(X1,Y1,Dir,Count1)) :-
    Count < 3, succ(Count, Count1),
    grid:move(X,Y, Dir, X1, Y1),
    grid:at(G, X1, Y1, _). % check that we are still within the grid

% Turn to direction
neighbor(G, s(X, Y, Dir, _), s(X1, Y1, Dir1, 1)) :-
    turn(Dir, Dir1),
    grid:move(X,Y, Dir1, X1, Y1),
    grid:at(G, X1, Y1, _).

neighbors(G, From, Neighbors) :- findall(N, neighbor(G, From, N), Neighbors).

% turn(FromDir, ToDir)
turn(l, d). turn(d, r). turn(r, u). turn(u, l).
turn(l, u). turn(u, r). turn(r, d). turn(d, l).

heat_at(G, X, Y, Heat) :- grid:at(G, X, Y, Ch), Heat is Ch - 48.
heat(G, s(X,Y,_,_), Heat) :- heat_at(G, X, Y, Heat).


score(Scores, Node, Score) :- (ht_get(Scores, Node, Score); Score=99999999).

came_from(CameFrom, Pos, []) :- \+ ht_get(CameFrom, Pos, _).
came_from(CameFrom, Pos, [From|Froms]) :-
    ht_get(CameFrom, Pos, From),
    came_from(CameFrom, From, Froms).

at_goal(s(X,Y,_,_), X-Y).

% dist from state to goal
distance(s(X1,Y1,_,_), X2-Y2, Dist) :- Dist is abs(X2-X1) + abs(Y2-Y1).

% When the first in open set is the goal, reconstruct path
astar(_, _, OpenSetIn, CameFrom, _, _, X-Y, [s(X,Y,end,0)|Path]) :-
    min_of_heap(OpenSetIn, _Priority, Current),
    at_goal(Current, X-Y), writeln(found_end(X-Y)), !,
    came_from(CameFrom, Current, Path).

astar(G, NeighborsGoal, OpenSetIn, CameFrom, GScores, FScores, Goal, Path) :-
    get_from_heap(OpenSetIn, _Priority, Current, OpenSet),
    \+ at_goal(Current, Goal),
    call(NeighborsGoal, G, Current, Neighbors),
    foldl({G,Current,CameFrom,GScores,FScores,Goal}/[Neighbor,Opens0,Opens1]>>(
              score(GScores, Current, CurGS), % current G score
              heat(G, Neighbor, Weight), % weight of moving from current to neighbor
              TentativeGS is CurGS + Weight,
              score(GScores, Neighbor, OldGS),
              %writeln(is_better(TentativeGS, OldGS)),
              (TentativeGS < OldGS
              % better than existing GS, replace
              -> ( ht_put(CameFrom, Neighbor, Current),
                   ht_put(GScores, Neighbor, TentativeGS),
                   distance(Neighbor, Goal, Heuristic),
                   FS is TentativeGS + Heuristic,
                   ht_put(FScores, Neighbor, FS),
                   % add neighbor to opens, if it isn't in it ( how to check membership? do we need to?)
                   add_to_heap(Opens0, FS, Neighbor, Opens1) )
              % not better than old
              ; ( Opens1 = Opens0 ))),
          Neighbors, OpenSet, OpenSet1),
    !,
    astar(G, NeighborsGoal, OpenSet1, CameFrom, GScores, FScores, Goal, Path).

astar(G, NeighborsGoal, Start, Goal, Path) :-
    ht_new(CameFrom), ht_new(GScores), ht_new(FScores),
    ht_put(GScores, Start, 0),
    distance(Start, Goal, Dist),
    ht_put(FScores, Start, Dist),
    list_to_heap([0-Start], OpenSet),
    astar(G, NeighborsGoal, OpenSet, CameFrom, GScores, FScores, Goal, Path).

part(NeighborsGoal, Ans) :-
    in(G), grid:size(G, W, H),
    astar(G, NeighborsGoal, s(1,1,r,0), W-H, Path),
    maplist([s(X,Y,_,_),X-Y]>>true, Path, Positions),
    sort(Positions, [_|Sorted]),
    maplist({G}/[X-Y,H]>>heat_at(G,X,Y,H), Sorted, Heat),
    sumlist(Heat, Ans).

part1(Ans) :- part(neighbors, Ans).
% part1(1023).

%% Part2 ULTRA CRUCIBLE! must move 4 blocks minimum and 10 blocks at most

% Can move forward
ultra(G, s(X, Y, Dir, Count), s(X1,Y1,Dir,Count1)) :-
    Count < 10, succ(Count, Count1),
    grid:move(X,Y, Dir, X1, Y1),
    grid:at(G, X1, Y1, _). % check that we are still within the grid

% Turn to direction, must have moved 4 blocks and that direction must have at least 4 positions
ultra(G, s(X, Y, Dir, C), s(X1, Y1, Dir1, 1)) :-
    C > 3,
    turn(Dir, Dir1),
    grid:move(X,Y, Dir1, X1, Y1),
    grid:move(X1,Y1, Dir1, X2, Y2),
    grid:move(X2,Y2, Dir1, X3, Y3),
    grid:move(X3,Y3, Dir1, X4, Y4),
    grid:at(G, X4, Y4, _). % 4 moves and still on the grid

ultra_neighbors(G, From, Neighbors) :- findall(N, ultra(G, From, N), Neighbors).

part2(Ans) :- part(ultra_neighbors, Ans).
% part2(1165).
