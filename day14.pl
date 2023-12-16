:- use_module(util).
:- use_module(library(clpfd)).
:- set_prolog_flag(stack_limit, 4_294_967_296).
parse(Codes, Cols) :- util:split_by(10, Codes, Rows),
                      transpose(Rows, Cols).
in(Cols) :- read_file_to_codes('day14.txt', Codes, []), parse(Codes, Cols).

% tilt line by moving rocks towards beginning, but not past # characters
tilt([],[]).
tilt(Line, Tilted) :-
    Line \= [],
    (append([Bef, [35|After]], Line), Sep=`#` ; Bef=Line,After=[],Sep=[]),
    msort(Bef, Sorted0), reverse(Sorted0,  Sorted1),
    tilt(After, AfterTilted),
    append([Sorted1, Sep, AfterTilted], Tilted).

tiltmap(In, Out) :- maplist(tilt, In, Out).

load(Col, Load) :-
    reverse(Col, ColR),
    foldl([C,I0-L0,I1-L1]>>( (C=79 -> L=I0; L=0),
                             I1 is I0 + 1,
                             L1 is L0 + L ),
          ColR, 1-0, _-Load).

loadsum(Cols, Load) :- foldl([C,L0,L1]>>(load(C, L), L1 is L0 + L), Cols, 0, Load).

part1(Ans) :-
    in(Cols),
    maplist(tilt, Cols, Tilted),
    loadsum(Tilted, Ans).
% part1(108889).

rotate_ccw(In, Out) :- transpose(In, Out0), reverse(Out0, Out).
tilt_and_rotate_ccw(In, Out) :- tiltmap(In, Tilted), rotate_ccw(Tilted, Out).

cycle(In,Out) :- iterate(4, tilt_and_rotate_ccw, In, Out).

run_cycles(G, Load) :-
    run_cycles_(G, [], 1_000_000_000, Load).

iterate(0, _, In,In).
iterate(N, Goal, In, Out) :- N > 0, succ(N1, N),
                             call(Goal, In, Out0),
                             iterate(N1, Goal, Out0, Out).

run_cycles_(G, Hashes, RoundsRemaining, Load) :-
    RoundsRemaining > 0,
    ThisRound is 1_000_000_000 - RoundsRemaining,
    cycle(G, G1),
    term_hash(G1, H),
    ( memberchk(PrevRound-H, Hashes)
    -> ( % found previous hash
        CycleLen is ThisRound-PrevRound,
        RoundsRem is (RoundsRemaining-1) mod CycleLen,
        iterate(RoundsRem, cycle, G1, GFinal), !,
        loadsum(GFinal, Load))
    ; ( RoundsRem is RoundsRemaining - 1, !,
        run_cycles_(G1, [ThisRound-H|Hashes], RoundsRem, Load) )).

part2(Ans) :- in(G), run_cycles(G, Ans).
% part2(104671).
