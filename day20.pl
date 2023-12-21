:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

%% Parsing

modname(Name) --> string_without(" ", NameCs), { atom_codes(Name, NameCs) }.
module_(broadcaster,broadcaster) --> "broadcaster".
module_(Name, flip_flop) --> "%", modname(Name).
module_(Name, conjunction) --> "&", modname(Name).
module(m(Name, Type, Outputs)) --> module_(Name,Type), " -> ",
                                   outputs(Outputs).
outputs([]) --> eol.
outputs([O|Outputs]) --> string_without(", \n", Cs), { atom_codes(O, Cs) }, more_outputs(Outputs).
more_outputs([]) --> eol.
more_outputs(Outputs) --> ",", blanks, outputs(Outputs).

modules([M|Ms]) --> module(M), modules(Ms).
modules([]) --> eos.

% Given a list of parsed modules, output modules with inputs and initial state
% as a dictionary, keyd by the module name
% [m(Name, Type, [Out...]),...] => modules{Name: m(Name,Type, [In...], [Out...], State),...}
initial_modules(Mods, ModsWithInputs) :-
    foldl({Mods}/[m(Name,Type,Outputs), M0, M1]>>(
              convlist({Name}/[m(N,_,Out),N]>>member(Name,Out), Mods, Inputs),
              initial_state(Type, Inputs, State),
              put_dict(Name, M0, m(Name,Type,Inputs,Outputs,State), M1) ),
          Mods, modules{low: 0, high: 0, sent_cycle: cycle{}}, ModsWithInputs).

initial_state(broadcaster, _, t).
initial_state(flip_flop, _, off).
initial_state(conjunction, Inputs, State) :- maplist([I,I=low]>>true, Inputs, State).

in(M) :- phrase_from_file(modules(Ms), 'day20.txt'), initial_modules(Ms, M).

mod(Mods, Name, M) :- get_dict(Name, Mods, M).

% Set the state of module by name
modstate(ModsIn, Name, State, ModsOut) :-
    get_dict(Name, ModsIn, m(Name,Type,In,Out,_)),
    put_dict(Name, ModsIn, m(Name,Type,In,Out,State), ModsOut).

modstate(Mods, Name, State) :-
    get_dict(Name, Mods, m(Name,_,_,_,State)).

% Increment a pulse
inc(M0, _-Key-_, M1) :- get_dict(Key, M0, V), V1 is V + 1, put_dict(Key, M0, V1, M1).
inc(M0, _-Key-_, M1) :- \+ get_dict(Key, M0, _), put_dict(Key, M0, 1, M1).

set_sent_cycle(M0, Name, Cycle, M1) :-
    get_dict(sent_cycle, M0, SentCycle0),
    put_dict(Name, SentCycle0, Cycle, SentCycle1),
    put_dict(sent_cycle, M0, SentCycle1, M1).

sent_cycle(M, Name, Cycle) :-
    get_dict(sent_cycle, M, SentCycle),
    get_dict(Name, SentCycle, Cycle).

flip(on,off,low).
flip(off,on,high).

send_to_out(From,Pulse,To, From-Pulse-To).

% handle(Mod, From, Pulse, StateOut, SendsOut)

handle(m(broadcaster, _, _, Outputs, State), _, P, State, Sends) :-
    maplist({P}/[O,broadcaster-P-O]>>true, Outputs, Sends).

handle(m(_, flip_flop, _, _, State), _, high, State, []).  % do nothing on high
handle(m(Name, flip_flop, _, Outputs, State0), _, low, State1, Sends) :-
    flip(State0, State1, P),
    maplist(send_to_out(Name, P), Outputs, Sends).

handle(m(Name, conjunction, _In, Outputs, State0), From, P, State1, Sends) :-
    select(From=_, State0, From=P, State1),
    conjunction_out(State1, POut),
    maplist(send_to_out(Name,POut), Outputs, Sends).

conjunction_out(State, POut) :-
    maplist([_=high]>>true, State)
    -> POut = low
    ;  POut = high.

% Send a pulses through the chain.
% This produces a new modified modules
send(_, M, [], M).
send(Record, ModsIn0, [From-Pulse-To|Pulses], ModsOut) :-
    %% instead of append, maplist all
    call(Record, ModsIn0, From-Pulse-To, ModsIn),
    ( mod(ModsIn, To, Recipient)
    -> ( handle(Recipient, From, Pulse, State, Sends),
         modstate(ModsIn, To, State, Mods1),
         append(Pulses, Sends, MorePulses) )
    ; ( Mods1 = ModsIn, MorePulses = Pulses )),
    send(Record, Mods1, MorePulses, ModsOut).

% Press button N times
button(0, Mods, Mods).
button(N, M0, ModsOut) :-
    N > 0,
    send(inc, M0, [button-low-broadcaster], M1),
    N1 is N - 1,
    button(N1, M1, ModsOut).

part1(Ans) :-
    in(Ms0),
    button(1000, Ms0, Mods),
    _{low: Lo, high: Hi} :< Mods,
    Ans is Lo * Hi.
% part1(825167435).

%% Part2 press button until low pulse is sent to rx
% last component until rx has 4 inputs... detect the cycle for each

sends_to(Ms, To, Sender) :-
    dict_pairs(Ms, modules, Pairs),
    member(Sender-m(_,_,_,Out,_), Pairs),
    memberchk(To, Out).

sync_modules(Ms, Mods) :-
    sends_to(Ms, rx, Main),
    findall(M, sends_to(Ms,Main,M), Mods).

record_cycle(SyncModules,Cycle, Main, M0, From-Pulse-To, M1) :-
    (( memberchk(From, SyncModules), Pulse = high, To = Main )
    -> ( writeln(sync_module(From-Pulse-To, cycle(Cycle))),
         set_sent_cycle(M0, From, Cycle, M1) )
    ; M1 = M0
    ), !.

done(M, CV) :- get_dict(sent_cycle, M, CV), dict_size(CV, 4).
cycles(_,_,_, M, Cycles) :-
    done(M, CycleVals),
    dict_pairs(CycleVals,cycle,CyclePairs),
    pairs_values(CyclePairs, Cycles).

cycles(SyncModules,Main,Cycle, M0, Cycles) :-
    \+ done(M0, _),
    send(record_cycle(SyncModules,Cycle,Main), M0, [button-low-broadcaster], M1),
    succ(Cycle,NextCycle),
    cycles(SyncModules,Main,NextCycle, M1, Cycles).

part2(Ans) :-
    in(Ms),
    sends_to(Ms, rx, Main),
    findall(M, sends_to(Ms, Main, M), SyncModules),
    writeln(sync_modules(SyncModules, main(Main))),
    cycles(SyncModules,Main,1,Ms, Cycles),
    foldl([C0,C1,P]>>(P is lcm(C0,C1)), Cycles, 1, Ans).
% part2(225514321828633).
