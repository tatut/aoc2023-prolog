:- use_module(library(dcg/basics)).

game(g(Id, GemSets)) --> `Game `, integer(Id), `: `, gemsets(GemSets), eol.
gemsets([Gems|GemSets]) --> gems(Gems), more_gemsets(GemSets).
more_gemsets([]) --> [].
more_gemsets(GemSets) --> `; `, gemsets(GemSets).
gems([]) --> [].
gems([Color=Count|Gs]) --> integer(Count), ` `, string_without(`,;\n`, ColorCs),
                           { atom_codes(Color, ColorCs) },
                           more_gems(Gs).
more_gems([]) --> [].
more_gems(Gs) --> `, `, gems(Gs).
games([]) --> eos.
games([G|Gs]) --> game(G), games(Gs).

in(Gs) :- phrase_from_file(games(Gs), 'day2.txt').

gem_count(Gs, Col, Count) :- member(Col=Count, Gs).
gem_count(Gs, Col, 0) :- \+ member(Col=_, Gs).

possible(g(_, GemSets)) :-
    \+ (member(Gs, GemSets),
        gem_count(Gs, red, Reds),
        gem_count(Gs, green, Greens),
        gem_count(Gs, blue, Blues),
        (Reds > 12; Greens > 13; Blues > 14)).

power(g(_, GemSets), Power) :-
    aggregate_all(max(R), (member(Gs, GemSets), gem_count(Gs, red, R)),
                  Reds),
    aggregate_all(max(G), (member(Gs, GemSets), gem_count(Gs, green, G)),
                  Greens),
    aggregate_all(max(B), (member(Gs, GemSets), gem_count(Gs, blue, B)),
                  Blues),
    Power is Reds * Greens * Blues.

part1(Ans) :-
    in(In),
    aggregate_all(sum(Id), (member(G, In),
                            G=g(Id,_),
                            possible(G)), Ans).

part2(Ans) :-
    in(In),
    maplist(power, In, Powers),
    sum_list(Powers, Ans).
