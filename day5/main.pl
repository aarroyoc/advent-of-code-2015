:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

data(Data) :-
    phrase_from_file(lines(Data), "input").

lines([]) --> [].
lines([X|Xs]) -->
    seq(X),
    "\n",
    lines(Xs).

part_one(X) :-
    once(data(Data)),
    setof(NiceStr, (member(NiceStr, Data),nice(NiceStr)), NiceStrs),
    length(NiceStrs, X).

part_two(X) :-
    once(data(Data)),
    setof(NiceStr, (member(NiceStr, Data),nice2(NiceStr)), NiceStrs),
    length(NiceStrs, X).

nice(Str) :-
    at_least_three_vowels(Str),
    one_letter_twice_in_a_row(Str),
    does_not_contain_banned(Str).

at_least_three_vowels(Str) :-
    findall(Vowel, (
		member(Vowel, "aeiou"),
		member(Vowel, Str)
	    ), Vowels),
    length(Vowels, N),
    N >= 3.

one_letter_twice_in_a_row(Str) :-
    substr(Str, [X, X]).

does_not_contain_banned(Str) :-
    \+ substr(Str, "ab"),
    \+ substr(Str, "cd"),
    \+ substr(Str, "pq"),
    \+ substr(Str, "xy").

substr(Str, Sub) :-
    append(_, X, Str),
    append(Sub, _, X).

nice2(Str) :-
    one_letter_repeated_with_one_between(Str),
    pair_twice_without_overlapping(Str).

one_letter_repeated_with_one_between(Str) :-
    substr(Str, [X, _, X]).

pair_twice_without_overlapping(Str) :-
    substr(Str, Sub),
    append([X,Y|_], [X, Y], Sub),
    length(Sub, N),
    N > 3.
