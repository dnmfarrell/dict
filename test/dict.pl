:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module('../src/dict').

test("add/get", (
  add(foo,D,1),
  add("bar",D,2),
  add(333,D,3),
  get(foo,D,1),
  get("bar",D,2),
  get(333,D,3)
)).

test("put", (
  add(foo,D,1),
  add("bar",D,2),
  add(333,D,3),
  put(foo,D,2,D1),
  put("bar",D1,3,D2),
  put(333,D2,4,D3),
  get(foo,D3,2),
  get("bar",D3,3),
  get(333,D3,4)
)).

test("to_list", (
  Ls = [c-3,f-1,a-2,b-2],
  add(b,D,2),
  add(f,D,1),
  add(a,D,2),
  add(c,D,3),
  to_list(D, Ls)
)).

test("height/balance", (
  EmptyTree = _,
  height(EmptyTree, -1),
  add(a,WorstTree,1),
  add(b,WorstTree,2),
  add(c,WorstTree,3),
  add(d,WorstTree,4),
  add(e,WorstTree,5),
  add(f,WorstTree,6),
  add(g,WorstTree,7),
  height(WorstTree, 6),
  balance(WorstTree, BalancedTree),
  height(BalancedTree, 2)
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
