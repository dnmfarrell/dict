:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module('../src/dict').

test("add/get", (
  V=1,
  add(foo,D,V),
  add(bar,D,2),
  add(goo,D,2),
  get(foo,D,V1),
  V=V1
)).

test("put", (
  V=1,
  add(foo,D,V),
  add(bar,D,2),
  add(goo,D,2),
  V1=2,
  put(foo,D,V1,D1),
  get(foo,D1,V2),
  V1=V2
)).

test("dict_list", (
  Ls = [c-3,f-1,a-2,b-2],
  add(b,D,2),
  add(f,D,1),
  add(a,D,2),
  add(c,D,3),
  dict_list(D, Ls)
)).

test("balance", (
  BalancedList = [g-1,e-1,f-1,c-1,a-1,b-1,d-1],
  keysort(BalancedList, WorstList),
  dict_list(WorstTree, WorstList),
  balance(WorstTree, BalancedTree),
  dict_list(BalancedTree, BalancedList)
)).

main :-
  findall(test(Name, Goal), test(Name, Goal), Tests),
  run_tests(Tests, Failed),
  show_failed(Failed),
  halt.

run_tests([], []).
run_tests([test(Name, Goal)|Tests], Failed) :-
  format("Running test \"~s\"~n", [Name]),
  (   call(Goal) ->
    Failed = Failed1
  ;   format("Failed test \"~s\"~n", [Name]),
    Failed = [Name|Failed1]
  ),
  run_tests(Tests, Failed1).

show_failed(Failed) :-
  phrase(portray_failed(Failed), F),
  format("~s", [F]).

portray_failed_([]) --> [].
portray_failed_([F|Fs]) -->
  "\"", F, "\"",  "\n", portray_failed_(Fs).

portray_failed([]) --> [].
portray_failed([F|Fs]) -->
  "\n", "Failed tests:", "\n", portray_failed_([F|Fs]).

:- initialization(main).
