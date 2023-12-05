:- use_module(library(dcg/basics)).

% Parsing
seeds([]) --> [].
seeds([S|Seeds]) --> integer(S), whites, seeds(Seeds).

seedsline(Seeds) --> `seeds: `, seeds(Seeds), eol.

maps([]) --> [].
maps([M|Maps]) --> map(M), maps(Maps).
map(m(Src, Dst, Ranges)) -->
    blanks,
    string_without(`-\n`, SrcCs), { atom_codes(Src, SrcCs) },
    `-to-`,
    string_without(` `, DstCs), { atom_codes(Dst, DstCs) },
    ` map:`, eol,
    ranges(Ranges).

ranges([]) --> [].
ranges([R|Ranges]) --> range(R), eol, ranges(Ranges).
range(r(DstStart-DstEnd,SrcStart-SrcEnd)) -->
    integer(DstStart), ` `, integer(SrcStart), ` `, integer(Len),
    { DstEnd is DstStart + Len - 1, SrcEnd is SrcStart + Len - 1}.

input(almanac(Seeds, Maps)) --> seedsline(Seeds), maps(Maps).

in(Input) :- phrase_from_file(input(Input), 'day5.txt').

% Part 1

mapping(almanac(_,Maps), SrcType, DstType, Ranges) :-
    member(m(SrcType, DstType, Ranges), Maps).

seed(almanac(Seeds,_), S) :- member(S, Seeds).

% Enumerate all types.
next_type(seed,soil).
next_type(soil,fertilizer).
next_type(fertilizer,water).
next_type(water,light).
next_type(light,temperature).
next_type(temperature,humidity).
next_type(humidity,location).
next_type(location,done). % for part2


% Convert where we find a suitable conversion range
convert(Almanac, SrcType, DstType, SrcValue, DstValue) :-
    mapping(Almanac, SrcType, DstType, Ranges),
    member(r(DstStart-_, SrcStart-SrcEnd), Ranges), % find range
    between(SrcStart, SrcEnd, SrcValue),
    SrcIdx is SrcValue - SrcStart,
    DstValue is DstStart + SrcIdx.

% Convert where we don't find a range, values are same
convert(Almanac, SrcType, DstType, V, V) :-
    mapping(Almanac, SrcType, DstType, Ranges),
    \+ ( member(r(_, SrcStart-SrcEnd), Ranges),
         between(SrcStart, SrcEnd, V) ).

% convert between types where no direct mapping exists
convert(Almanac, SrcType, DstType, SrcValue, DstValue) :-
    \+ mapping(Almanac, SrcType, DstType, _), % no mapping
    next_type(SrcType, IntType), % intermediate type
    convert(Almanac, SrcType, IntType, SrcValue, IntValue),
    convert(Almanac, IntType, DstType, IntValue, DstValue).

part1(Ans) :-
    in(Almanac),
    aggregate_all(min(Loc), (seed(Almanac, S),
                             convert(Almanac, seed, location, S, Loc)),
                  Ans).

% part1(227653707).


% Part 2 track ranges

seedranges(almanac([],_),[]).
seedranges(almanac([Start,Len|Seeds],_), [Start-End|Ranges]) :-
    End is Start + Len - 1,
    seedranges(almanac(Seeds,_),Ranges).

overlaps(R1s-_, R2s-R2e) :- between(R2s, R2e, R1s).
overlaps(_-R1e, R2s-R2e) :- between(R2s, R2e, R1e).
overlaps(R1s-R1e, R2s-_) :- between(R1s, R1e, R2s).
overlaps(R1s-R1e, _-R2e) :- between(R1s, R1e, R2e).

% No matching range in mappings, the range maps to itself
mapranges(Mappings, Range, [Range]) :-
    \+ ( maplist([r(_,Src),Src]>>true, Mappings, Sources),
         member(S, Sources),
         overlaps(S, Range) ).

% With Mappings being all the mappings, and r(..) being a single mapping
% and Rs-Re being a range to map, succeed with Start-End being
% the part before, the part after, or the common part.
mapped_range_part(_, r(DstS-_,SrcS-SrcE), Rs-Re, Start-End) :-
    overlaps(SrcS-SrcE, Rs-Re), % this overlaps
    Lo is max(SrcS, Rs),
    Hi is min(SrcE, Re),
    Shift is DstS - SrcS,
    Lo =< Hi,
    Start is Lo + Shift, End is Hi + Shift.

% The before part, recursively map it
mapped_range_part(Mappings, r(_,SrcS-SrcE), Rs-Re, Before) :-
    overlaps(SrcS-SrcE, Rs-Re), Rs < SrcS, % this overlaps and starts before
    SrcS1 is SrcS - 1,
    mapped_range(Mappings, Rs-SrcS1, Before).

% The after part, recursively map it
mapped_range_part(Mappings, r(_,SrcS-SrcE), Rs-Re, After) :-
    overlaps(SrcS-SrcE, Rs-Re), SrcE < Re, % this overlaps and ends after
    SrcE1 is SrcE + 1,
    mapped_range(Mappings, SrcE1-Re, After).

mapped_range(Mappings, Range0, Range0) :- \+ (member(r(_,Src),Mappings), overlaps(Src,Range0)).
mapped_range(Mappings, Range0, Range1) :-
    member(M, Mappings),
    mapped_range_part(Mappings, M, Range0, Range1), !.


% Map the current ranges for Src->Dst and recurse to the next type
solve(Almanac, SrcType, DstType, Ranges, Ans) :-
    mapping(Almanac, SrcType, DstType, Mappings),
    findall(R1, (member(R0,Ranges),mapped_range(Mappings,R0,R1)), NewRanges),
    next_type(DstType, NextType),
    solve(Almanac, DstType, NextType, NewRanges, Ans).

% Final step, sort ranges and get start
solve(_, location, _, Rs, Out) :- sort(Rs,[Out-_|_]).

part2(Ans) :- in(A), seedranges(A, Seeds), solve(A, seed, soil, Seeds, Ans).
% part2(78775051).
