:- use_module(util).
:- use_module(library(clpfd)).

parse(Codes, Cols) :- util:split_by(10, Codes, Rows),
                      transpose(Rows, Cols).
in(Cols) :- read_file_to_codes('day14.txt', Codes, []), parse(Codes, Cols).

% Tilt any rocks towards the beginning of the column
% round  O  79
% empty  .  46
% cube   #  35

empty(46).

% swap .O to O.
swap(46, 79).

tilt_(Idx, Line) :- succ(Idx, Idx1), \+ arg(Idx1, Line, _).
tilt_(Idx, Line) :-
    succ(Idx, Idx1),
    arg(Idx, Line, C1),
    arg(Idx1, Line, C2),
    (swap(C1,C2)
    -> ( nb_setarg(Idx, Line, C2),
         nb_setarg(Idx1, Line, C1) ); true),
    tilt_(Idx1, Line).


tilt(In, Out) :-
    compound_name_arguments(Line, line, In),
    tilt_(1,Line),
    compound_name_arguments(Line, line, Out1),
    (In = Out1 -> Out = Out1; tilt(Out1, Out)).

tiltmap(In, Out) :- maplist(tilt, In, Out).

load(Col, Load) :-
    reverse(Col, ColR),
    foldl([C,I0-L0,I1-L1]>>( (C=79 -> L=I0; L=0),
                             I1 is I0 + 1,
                             L1 is L0 + L ),
          ColR, 1-0, _-Load).

part1(Ans) :-
    %ex1(E), parse(E, Cols),
    in(Cols),
    maplist(tilt, Cols, Tilted),
    maplist(load, Tilted, Load),
    sum_list(Load, Ans).
% part1(108889).


% For part2 we will be doing a plethora of tilts, so
% let's do a compound term mutation instead of forever
% transposing the lists
%
% model grid as compound term g(Width,Height,data(..Data))
% where Data has all data left to right, top to bottom.

parse2(Codes, g(W,H,Data)) :-
    util:split_by(10, Codes, Lines),
    length(Lines, H),
    [L|_]=Lines,
    length(L, W),
    append(Lines, Codes1),
    compound_name_arguments(Data, data, Codes1).

in2(G) :- read_file_to_codes('day14.txt', Codes, []), parse2(Codes, G).

dir(north, 0, 1).
dir(south, 0, -1).
dir(west, 1, 0).
dir(east, -1, 0).

size(g(W,H,_), W, H).
pos(g(W,H,_), X, Y, Pos) :- between(1,W,X), between(1,H,Y), Pos is (Y-1) * W + X.
at(g(_,_,Data), Pos, C) :- arg(Pos, Data, C).
set_at(g(_,_,Data), Pos, C) :- nb_setarg(Pos, Data, C).

at(G, X, Y, C) :- pos(G, X, Y, Pos), at(G, Pos, C).
set_at(G, X, Y, C) :- pos(G, X, Y, Pos), set_at(G, Pos, C).

% Mutating tilt!
% west/east, loop each line
tiltmap2(G, Dir) :- dir(Dir, Dx, 0),
                    size(G, W, H),
                    (Dx > 0 -> Start = 1; Start = W),
                    util:dotimes(1,H, {G,Start,Dx}/[Y]>>tiltg(G, Start, Y, Dx, 0)).

% north/south, loop each column
tiltmap2(G, Dir) :- dir(Dir, 0, Dy),
                    size(G, W, H),
                    (Dy > 0 -> Start = 1; Start = H),
                    util:dotimes(1,W,
                                 {G,Start,Dy}/[X]>>tiltg(G, X, Start, 0, Dy)).

% Get next position
next(X,Y,Dx,Dy,X1,Y1) :- X1 is X+Dx, Y1 is Y+Dy.

% No next position, we are done for this row/col
tiltg(G, X, Y, Dx, Dy, 0) :- next(X,Y,Dx,Dy,X1,Y1), \+ at(G, X1,Y1, _).
tiltg(G, X, Y, Dx, Dy, Swaps) :-
    next(X,Y,Dx,Dy,X1,Y1),
    at(G, X, Y, C1),
    at(G, X1, Y1, C2),
    (swap(C1, C2) ->
         ( set_at(G, X, Y, C2),
           set_at(G, X1, Y1, C1), Swaps1 = 1)
    ; Swaps1 = 0),
    tiltg(G, X1, Y1, Dx, Dy, Swaps2),
    Swaps is Swaps1 + Swaps2.

tiltg(G, X, Y, Dx, Dy) :-
    tiltg(G, X, Y, Dx, Dy, Swaps),
    (Swaps > 0 -> tiltg(G,X,Y,Dx,Dy); true).

% tilt north, west, south, east
% output is again oriented towards north
cycle(G) :-
    tiltmap2(G, north),
    tiltmap2(G, west),
    tiltmap2(G, south),
    tiltmap2(G, east).

show(G) :-
    size(G, W, H),
    util:repeat(W, `-`, Sep),
    append([[43],Sep,[43]], SepCs),
    string_codes(SepStr, SepCs),
    writeln(SepStr),
    util:dotimes(1,H,
                 {G,W}/[Y]>>( numlist(1, W, Xs),
                              maplist({G,Y}/[X,Ch]>>at(G,X,Y,Ch), Xs, Cs),
                              append([[124],Cs,[124]], Cs1),
                              string_codes(Str, Cs1),
                              writeln(Str) )),
    writeln(SepStr), writeln("").

% run one billion (1 000 000 000) cycles!
% detect where the hash of an iteration matches a previous one
% then we can only run RoundsRemaining mod CycleLen

load2(G, Load) :-
    size(G, W, H),
    aggregate_all(sum(L),
                  (between(1,W,X), between(1,H,Y), at(G,X,Y,79),
                   L is H - Y + 1),
                  Load).

run_cycles(G, Load) :-
    ht_new(Hash),
    run_cycles_(G, Hash, 1_000_000_000, Load).


run_cycles_(G, Hashes, RoundsRemaining, Load) :-
    RoundsRemaining > 0,
    ThisRound is 1_000_000_000 - RoundsRemaining,
    cycle(G),
    term_hash(G, H),
    ( ht_get(Hashes, H, PrevRound)
    -> ( % found previous hash
        writeln(found_same_hash(this(ThisRound), prev(PrevRound))),
        RoundsRem is (RoundsRemaining-1) mod (ThisRound-PrevRound),
        writeln(modulo_rounds_rem(RoundsRem)),
        !,
        util:dotimes(1,RoundsRem, [_]>>cycle(G)),
        load2(G, Load))
    ; ( ht_put(Hashes, H, ThisRound),
        RoundsRem is RoundsRemaining - 1, !,
        run_cycles_(G, Hashes, RoundsRem, Load) )).


% ex1(E), parse2(E, G), show(G), cycle(G), show(G).
part2(Ans) :-
    in2(G), run_cycles(G, Ans).

% part2(104671).
