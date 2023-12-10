:- dynamic(start/1).

in(G) :- read_file_to_string('day10.txt', Str, []), parse(Str, G).

parse(Str, G) :-
    string_lines(Str, Lines),
    Lines=[Line|_],
    string_length(Line, W),
    length(Lines, H),
    append([W,H], Lines, Args),
    compound_name_arguments(G, g, Args).

atc(G, X-Y, Ch) :-
    width(G,W), height(G,H), between(1,W,X), between(1,H,Y),
    Idx is Y + 2,
    arg(Idx, G, Line),
    string_code(X, Line, Ch).

at(G, X-Y, Pipe) :- atc(G, X-Y, Ch), pipe(Ch, Pipe).

ground(G, X-Y) :-
    Idx is Y + 2,
    arg(Idx, G, Line),
    string_code(X, Line, 46).

pipe(124, [n,s]).   % | is a vertical pipe connecting north and south.
pipe(45, [w,e]).    % - is a horizontal pipe connecting east and west.
pipe(76, [n,e]).    % L is a 90-degree bend connecting north and east.
pipe(74, [n,w]).    % J is a 90-degree bend connecting north and west.
pipe(55, [s,w]).    % 7 is a 90-degree bend connecting south and west.
pipe(70, [s,e]).    % F is a 90-degree bend connecting south and east.
pipe(83, S) :- start(S).  % S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

% Move from pos1 to pos2 in direction, if there is an accepting pipe
to(G, X1-Y1, n, X1-Y2) :- Y2 is Y1 - 1, at(G, X1-Y2, P), memberchk(s, P).
to(G, X1-Y1, s, X1-Y2) :- Y2 is Y1 + 1, at(G, X1-Y2, P), memberchk(n, P).
to(G, X1-Y1, w, X2-Y1) :- X2 is X1 - 1, at(G, X2-Y1, P), memberchk(e, P).
to(G, X1-Y1, e, X2-Y1) :- X2 is X1 + 1, at(G, X2-Y1, P), memberchk(w, P).

width(G,W) :- arg(1, G, W).
height(G,H) :- arg(2, G, H).

find_start(G, X-Y) :-
    width(G, W), height(G, H),
    between(1,W,X), between(1,H,Y),
    atc(G, X-Y, 83).

set_start(Type) :- retractall(start(_)), asserta(start(Type)).

% Travel through the pipeline until you reach the start point again.
connected_pipe(_, At, _, At, P, P) :- length(P, S), S > 2. % end when at start again

connected_pipe(G, Start, Visited, At, Acc, Final) :-
    add_nb_set(At, Visited, true),
    at(G, At, Pipe),
    % Try to go to each direction
    member(Dir, Pipe),
    to(G, At, Dir, NewAt),
    connected_pipe(G, Start, Visited, NewAt, [NewAt|Acc], Final).

find_connected_pipe(G, Pipe) :-
    find_start(G, Start),
    member(PipeType, [[n,s],[w,e],[n,e],[n,w],[s,w],[s,e]]),
    set_start(PipeType),
    empty_nb_set(Visited),
    connected_pipe(G, Start, Visited, Start, [], Pipe).

part1(Ans) :-
    in(G),
    find_connected_pipe(G, Pipe),
    length(Pipe, L),
    Ans is L / 2.

% part1(7173).

%% Part2 shoelace formula to calculate area
area([_], Acc, Area) :- Area is Acc / 2.
area([X1-Y1,X2-Y2|Points], Acc0, Area) :-
    Acc is Acc0 + ((X1 * Y2) - (X2 * Y1)),
    area([X2-Y2|Points], Acc, Area).

area(Pipe0, Area) :-
    Pipe0=[Start|Rest],
    append(Pipe0, [Start], Pipe),
    area(Pipe, 0, Area).

part2(Ans) :-
    in(G),
    find_connected_pipe(G, P),
    reverse(P, Pr),
    area(Pr, Area),
    width(G, W), height(G, H),
    length(P, Len),
    Ans is Area - Len/2 + 1.

% part2(291).
