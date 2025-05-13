% use_case_to_plantuml.pl
% Author: Your Name
% Date: May 2025
% Description: Converts UseCaseDG specifications into PlantUML

% -------------------------------
% 1. ACTORS AND USE CASES
% -------------------------------

% Actors
actor(passenger).
actor(minor_passenger).
actor(passenger_with_special_needs).
actor(tour_guide).

% Use Cases
use_case(individual_check_in).
use_case(counter_check_in).
use_case(kiosk_check_in).
use_case(group_check_in).
use_case(baggage_check_in).
use_case(security_screening).
use_case(baggage_handling).

% -------------------------------
% 2. ALIASES (GROUPED TOGETHER)
% -------------------------------

alias(passenger, 'P').
alias(minor_passenger, 'MP').
alias(passenger_with_special_needs, 'PSN').
alias(tour_guide, 'TG').
alias(individual_check_in, 'IC').
alias(counter_check_in, 'CC').
alias(kiosk_check_in, 'KC').
alias(group_check_in, 'GC').
alias(baggage_check_in, 'BC').
alias(security_screening, 'SS').
alias(baggage_handling, 'BH').

% -------------------------------
% 3. PACKAGES AND USE CASE GROUPING
% -------------------------------

package(check_in).

use_case_in_package(individual_check_in, check_in).
use_case_in_package(counter_check_in, check_in).
use_case_in_package(kiosk_check_in, check_in).
use_case_in_package(group_check_in, check_in).
use_case_in_package(baggage_check_in, check_in).
use_case_in_package(security_screening, check_in).
use_case_in_package(baggage_handling, check_in).

% -------------------------------
% 4. RELATIONSHIPS
% -------------------------------

% Generalization
generalization(minor_passenger, passenger).
generalization(passenger_with_special_needs, passenger).
generalization(tour_guide, passenger).
generalization(counter_check_in, individual_check_in).
generalization(kiosk_check_in, individual_check_in).

% Include / Extend
include(group_check_in, individual_check_in).
extend(baggage_check_in, individual_check_in).
extend(baggage_handling, counter_check_in).
extend(baggage_handling, kiosk_check_in).

% Associations
association(tour_guide, group_check_in).
association(passenger, individual_check_in).
association(passenger, security_screening).

% -------------------------------
% 5. VALIDATION PREDICATES
% -------------------------------

valid_generalization(X, Y) :- (actor(X); use_case(X)), (actor(Y); use_case(Y)).
valid_include(From, To) :- use_case(From), use_case(To).
valid_extend(From, To) :- use_case(From), use_case(To).
valid_association(Actor, UC) :- actor(Actor), use_case(UC).

% -------------------------------
% 6. COUNT USE CASES IN A PACKAGE
% -------------------------------

count_use_cases(Package, Count) :-
    findall(UC, use_case_in_package(UC, Package), List),
    length(List, Count).

% -------------------------------
% 7. PLANTUML GENERATION
% -------------------------------

write_line(Line) :- write(Line), nl.

% Convert atoms to readable labels with newlines
atom_concat_words(Atom, Label) :-
    atomic_list_concat(Words, '_', Atom),
    maplist(capitalize, Words, CapWords),
    atomic_list_concat(CapWords, ' ', Flat),
    reformat_label(Flat, Label).

capitalize(Word, Capitalized) :-
    atom_chars(Word, [H|T]),
    upcase_atom(H, UH),
    atom_chars(Capitalized, [UH|T]).

reformat_label(In, Out) :-
    split_string(In, " ", "", Words),
    length(Words, Len),
    ( Len > 1 ->
        append(Front, [Last], Words),
        atomic_list_concat(Front, ' ', BeforeLast),
        format(string(Out), "~w\\\\n~w", [BeforeLast, Last])
    ; Out = In
    ).

% Generate actor definitions
generate_actors :-
    forall(actor(A), (
        alias(A, Alias),
        atom_concat_words(A, Label),
        format('actor "~w" as ~w~n', [Label, Alias])
    )).

% Generate use case definitions in a rectangle
generate_use_cases :-
    write_line('rectangle "<<Business>>\\\\nAirport" {'),
    forall(use_case(U), (
        alias(U, Alias),
        atom_concat_words(U, Label),
        format('  usecase "~w" as ~w~n', [Label, Alias])
    )),
    write_line('}').

% Generate relationships
generate_relationships :-
    forall(generalization(Sub, Super), (
        alias(Sub, A1), alias(Super, A2),
        format('~w --|> ~w~n', [A1, A2])
    )),
    forall(include(From, To), (
        alias(From, A1), alias(To, A2),
        format('~w ..> ~w : <<include>>~n', [A1, A2])
    )),
    forall(extend(From, To), (
        alias(From, A1), alias(To, A2),
        format('~w ..> ~w : <<extend>>~n', [A1, A2])
    )),
    forall(association(A, UC), (
        alias(A, AA), alias(UC, AU),
        format('~w -- ~w~n', [AA, AU])
    )).

% Main generation function
generate_plantuml :-
    write_line('@startuml'),
    write_line('left to right direction'),
    write_line('skinparam packageStyle rectangle'),
    generate_actors,
    generate_use_cases,
    generate_relationships,
    write_line('center footer uml-diagrams.org'),
    write_line('@enduml').
