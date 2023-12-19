:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

%%% Parsing
atom_without(Without, Atom) -->
    string_without(Without, AtomCs),
    { length(AtomCs, L), L > 0, atom_codes(Atom, AtomCs) }.

action(accept) --> "A".
action(reject) --> "R".
action(to(Name)) --> atom_without("AR},:", Name).

workflows([]) --> eol.
workflows([Wf|Workflows]) --> workflow(Wf), eol, workflows(Workflows).
workflow(wf(Name,Rules)) --> atom_without("{", Name), "{", rules(Rules), "}".
rules([R|Rules]) --> rule(R), more_rules(Rules).
more_rules([]) --> [].
more_rules(Rules) --> ",", rules(Rules).

rule(r(If,Then)) --> if(If), ":", action(Then).
rule(r(true,Then)) --> action(Then).

if(if(Attr,Op,Val)) --> atom_without("<>:}", Attr), op_(Op), integer(Val).
op_(<) --> "<".
op_(>) --> ">".

part([x=X,m=M,a=A,s=S]) --> "{x=",integer(X),",m=",integer(M),
                            ",a=", integer(A),",s=",integer(S), "}".
parts([]) --> eos.
parts([P|Parts]) --> part(P), eol, parts(Parts).

input(Workflows,Parts) --> workflows(Workflows), parts(Parts).

in(Wfs,Ps) :- phrase_from_file(input(Wfs,Ps), 'day19.txt').

%%% Evaluation

% Evaluate single rule
eval(_, true).
eval(Attrs, if(Attr, Op, Val)) :- member(Attr=V, Attrs), call(Op, V, Val), !.

% Evaluate workflow, succeeds with first rule that matches (or fails)
eval_wf_([r(If,Action)|_], Attrs, Action) :- eval(Attrs, If).
eval_wf_([r(If,_)|Rules], Attrs, Action) :- \+ eval(Attrs, If),
                                            eval_wf_(Rules, Attrs, Action).

eval_wf(wf(_,Rules), Attrs, Action) :- eval_wf_(Rules,Attrs,Action).

wf_by_name(Workflows, Name, Wf) :- member(Wf, Workflows),
                                   Wf=wf(Name,_).

eventually_accepted(Workflows, Part) :-
    wf_by_name(Workflows, in, Start),
    eventually_accepted(Workflows, Start, Part).

eventually_accepted(_, Wf, Part) :- eval_wf(Wf, Part, accept).
eventually_accepted(_, Wf, Part) :- eval_wf(Wf, Part, reject), fail.

eventually_accepted(Workflows, Wf, Part) :-
    eval_wf(Wf, Part, to(Name)),
    wf_by_name(Workflows, Name, Next),
    eventually_accepted(Workflows, Next, Part).

rating_sum([x=X,m=M,a=A,s=S], Sum) :- Sum is X+M+A+S.

part1(Answer) :-
    in(Wfs,Ps),
    convlist({Wfs}/[P,P]>>eventually_accepted(Wfs,P), Ps, Accepted),
    maplist(rating_sum, Accepted, Ratings),
    sum_list(Ratings, Answer).
% part1(391132).

%% Part 2 count the number of distinct paths
%% any attr can have value from 1 - 4000

all_accepted_paths(Wfs, AllAcceptedCount) :-
    wf_by_name(Wfs, in, wf(_,Rules)),
    Remaining = [x=1-4000,m=1-4000,a=1-4000,s=1-4000],
    aggregate_all(sum(Accepted),
                  accepted_paths(Wfs, Rules, Remaining, Accepted),
                  AllAcceptedCount).

% Calculate possibilities from ranges
sum_count([], 1).
sum_count([_=Low-High|Rest], Count) :-
    sum_count(Rest, Count0),
    Count is (High-Low+1)*Count0.

% Update matching counts based on condition
matching(true, Counts, Counts). % all pass
matching(if(Attr, >, Val), Counts0, Counts1) :-
    select(Attr=Low0-High0, Counts0,
           Attr=Low1-High0, Counts1),
    succ(Val, Val1),
    Low1 is max(Low0,Val1),
    Low1 =< High0.

matching(if(Attr, <, Val), Counts0, Counts1) :-
    select(Attr=Low0-High0, Counts0,
           Attr=Low0-High1, Counts1),
    succ(Val1, Val),
    High1 is min(High0, Val1),
    Low0 =< High1.

non_matching(if(Attr, <, Val), Ranges, NonMatching) :-
    succ(Val1, Val),
    matching(if(Attr, >, Val1), Ranges, NonMatching).
non_matching(if(Attr, >, Val), Ranges, NonMatching) :-
    succ(Val, Val1),
    matching(if(Attr, <, Val1), Ranges, NonMatching).

% Accept: succeeds with the matching count
% and recurses with the non-matching
accepted_paths(_, [r(If,accept)|_], Ranges, Count) :-
    matching(If, Ranges, Matching),
    sum_count(Matching, Count).
accepted_paths(Wfs, [r(If,accept)|Rest], Ranges, Count) :-
    non_matching(If, Ranges, NonMatching),
    accepted_paths(Wfs, Rest, NonMatching, Count).

% Reject: just recurse non-matching
accepted_paths(Wfs, [r(If,reject)|Rest], Ranges, Count) :-
    % we need to continue with the non-matching
    non_matching(If, Ranges, NonMatching),
    accepted_paths(Wfs, Rest, NonMatching, Count).

% To: recurse matching to new rules and non-matching to rest
accepted_paths(Wfs, [r(If,to(Name))|_], Ranges, Count) :-
    matching(If, Ranges, Matching),
    wf_by_name(Wfs, Name, wf(_,Rules)),
    accepted_paths(Wfs, Rules, Matching, Count).

accepted_paths(Wfs, [r(If,to(_))|Rest], Ranges, Count) :-
    non_matching(If, Ranges, NonMatching),
    accepted_paths(Wfs, Rest, NonMatching, Count).

part2(Ans) :-
    in(Wfs, _),
    all_accepted_paths(Wfs, Ans).
% part2(128163929109524).
