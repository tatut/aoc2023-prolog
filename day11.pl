:- use_module(util).
:- dynamic expansion_factor/1.
:- dynamic empty_row/1.
:- dynamic empty_col/1.

in(G) :- read_file_to_string('day11.txt', Str, []), parse(Str, G).

parse(Str, G) :-
    string_lines(Str, Lines0),
    Lines0=[L0|_],
    string_length(L0, L),
    numlist(1, L, Idxs),
    util:enumerate(Lines0, Lines),
    maplist({Idxs}/[Y-Line, Gs]>>(convlist({Line,Y}/[X,X-Y]>>string_code(X,Line,35), Idxs, Gs)),
            Lines, Gs),
    append(Gs, AllGs),
    list_to_ord_set(AllGs, G).

is_empty_col(Gs, X) :- \+ memberchk(X-_, Gs).
is_empty_row(Gs, Y) :- \+ memberchk(_-Y, Gs).

prepare(Gs) :-
    pairs_keys(Gs, Xs), pairs_keys(Gs, Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    retractall(empty_row(_)),
    retractall(empty_col(_)),
    foreach((between(Xmin,Xmax,X), is_empty_col(Gs,X)),
            asserta(empty_col(X))),
    foreach((between(Ymin,Ymax,Y), is_empty_row(Gs,Y)),
            asserta(empty_row(Y))).


col_dist(X, 1) :- \+ empty_col(X).
col_dist(X, N) :- empty_col(X), expansion_factor(N).
row_dist(Y, 1) :- \+ empty_row(Y).
row_dist(Y, N) :- empty_row(Y), expansion_factor(N).

pairs(Gs, Pairs) :-
    findall(Pair, (select(G0, Gs, R), select(G1, R, _),
                   sort([G0,G1], Pair)), Pairs0),
    sort(Pairs0, Pairs).

distances(Gs, Dist) :-
    pairs(Gs, Pairs),
    length(Pairs, Len), writeln(pairs(Len)),
    maplist([[G0,G1],D]>>dist(G0,G1,D), Pairs, Dist).

xdist(X, X, 0).
xdist(X1,X2, D) :-
    \+ X1 = X2,
    sort([X1,X2], [Xsmall,Xbig]),
    Xb is Xbig - 1,
    numlist(Xsmall,Xb, Xlist),
    maplist(col_dist, Xlist, Xdists),
    sum_list(Xdists, D).

ydist(Y, Y, 0).
ydist(Y1, Y2, D) :-
    \+ Y1 = Y2,
    sort([Y1,Y2], [Ysmall,Ybig]),
    Yb is Ybig - 1,
    numlist(Ysmall,Yb, Ylist),
    maplist(row_dist, Ylist, Ydists),
    sum_list(Ydists, D).

dist(X1-Y1,X2-Y2,Dist) :-
    xdist(X1,X2, Xd),
    ydist(Y1,Y2, Yd),
    Dist is Xd+Yd.

set_expansion_factor(N) :-
    retractall(expansion_factor(_)),
    asserta(expansion_factor(N)).

part(ExpFactor, Ans) :-
    in(G), prepare(G), set_expansion_factor(ExpFactor),
    writeln(prepared),
    distances(G, Dists),
    sum_list(Dists, Ans).

part1(Ans) :- part(2, Ans).
part2(Ans) :- part(1000000, Ans).
