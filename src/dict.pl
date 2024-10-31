/*
    A dictionary inspired by The Art of Prolog

    Stores Key-Value pairs in an incomplete binary tree.

    Copyright 2024 David Farrell

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

*/

:- module(dict, [add/3, balance/2, get/3, height/2, put/4, to_list/2]).
:- use_module(library(lists)).

% thanks Sterling and Shapiro ❤
% Program 15.9 from the Art of Prolog
%
% "if the key is non-numeric, the predicates < and > must be generalized"
% Now we have compare/3, and the pair-type:
%
%lookup(Key, dict(Key-Value1, _, _), Value) :-
%  !, Value = Value1.
%
%lookup(Key, dict(Key1-_, Left, _), Value) :-
%  compare(<, Key, Key1), lookup(Key, Left, Value).
%
%lookup(Key, dict(Key1-_, _, Right), Value) :-
%  compare(>, Key, Key1), lookup(Key, Right, Value).

% Using a single rule and if/then avoids leaving choicepoints behind
% but checking for key existence versus adding a key is tricky, so we
% provide get/3 and add/3 for those cases.
lookup(Key, dict(Key1-Value1, Left, Right), Value) :-
  (   Key = Key1 ->
      Value = Value1
  ;   compare(<, Key, Key1) ->
      lookup(Key, Left, Value)
  ;   lookup(Key, Right, Value)
  ).

% fails if key doesn't exist, harder to get wrong
get(Key, Dict, Value) :-
  lookup(Key, Dict, Value), nonvar(Value).

% semantically clearer than lookup
add(Key, Dict, Value) :-
  lookup(Key, Dict, Value).

% sometimes we need to update a value in-place, which requires a new dict.
put(Key, dict(Key1-Value1, Left, Right), Value, Dict1) :-
  (   Key = Key1 ->
      Dict1 = dict(Key-Value, Left, Right)
  ;   compare(<, Key, Key1) ->
      put(Key, Left, Value, Dict2),
      Dict1 = dict(Key1-Value1, Dict2, Right)
  ;   put(Key, Right, Value, Dict2),
      Dict1 = dict(Key1-Value1, Left, Dict2)
  ).

% serialize a dict to a list of pairs
% Since we're using the pair-type, other functions like keysort/2
% work on the list...
to_list(D,Ls) :-
  to_list_([D|Hole],Hole,[],Ls).

to_list_([D|Ds],Hole,Acc,Ls) :-
  dict(Root,Left,Right) = D,
  (   Left = nil ->
      Hole = Hole1
  ;   Hole = [Left|Hole1]
  ),
  (   Right = nil ->
      Hole1 = Hole2
  ;   Hole1 = [Right|Hole2]
  ),
  (   Ds = [] ->
      Ls=[Root|Acc]
  ;   to_list_(Ds,Hole2,[Root|Acc],Ls)
  ).

% ...and since we can sort our pairs so easily, balancing the tree is trivial
balance(D, D1) :-
  nonvar(D),
  to_list(D, Ls),
  keysort(Ls, Ms),
  balance_([D1-Ms]).

balance_([]).
balance_([D-Ls|Tasks]) :-
  (   Ls = [] ->
      balance_(Tasks)
  ;   divide(Ls, Left, [Root|Right]),
      D = dict(Root, LeftD, RightD),
      Ms = [LeftD-Left|Tasks],
      Ns = [RightD-Right|Ms],
      balance_(Ns)
  ).

divide(Ls, Left, Right) :-
  list_midpoint(Ls, Mid),
  list_split(Ls, Mid, Left, Right).

list_midpoint([], _) :- false.
list_midpoint(Ls, Midpoint) :-
  length(Ls, Len),
  Midpoint is Len // 2.

list_split([], _, [], []).
list_split(Ls, I, Ms, Ns) :-
  nth0(I, Ls, E),
  Ns = [E|_],
  append(Ms, Ns, Ls),
  !.

height(D,H) :-
  height_([-1-D|Hole],Hole,H).

height_([Acc-D|Tasks],Hole,H) :-
  (   var(D) ->
      (   Tasks = [] ->
          H is Acc
      ;   height_(Tasks,Hole,H)
      )
  ;   dict(_, Left, Right) = D,
      Acc1 is Acc + 1,
      Hole = [Acc1-Left, Acc1-Right|Hole1],
      height_(Tasks,Hole1,H)
  ).
