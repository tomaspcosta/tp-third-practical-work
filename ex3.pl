% -------------------------------
% 1. ACTORS AND USE CASES
% -------------------------------

% Define actors involved in the system
% These are the participants in the use case diagram
actor(passenger).
actor(minor_passenger).
actor(passenger_with_special_needs).
actor(tour_guide).

% Define the use cases in the system
% These are the functionalities or actions that the system performs
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

% Aliases are defined for both actors and use cases
% These make the Prolog code more concise and improve readability in the PlantUML output
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

% Define the package to which use cases belong
% In this case, all use cases are grouped under the 'check_in' package
package(check_in).

% Associate each use case with the 'check_in' package
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

% Generalizations define hierarchical relationships between actors or use cases
% Example: a 'minor_passenger' is a type of 'passenger'
generalization(minor_passenger, passenger).
generalization(passenger_with_special_needs, passenger).
generalization(tour_guide, passenger).
generalization(counter_check_in, individual_check_in).
generalization(kiosk_check_in, individual_check_in).

% Include and Extend relationships between use cases
% Include: indicates that one use case always includes another
% Extend: indicates that one use case extends another under certain conditions
include(group_check_in, individual_check_in).
extend(baggage_check_in, individual_check_in).
extend(baggage_handling, counter_check_in).
extend(baggage_handling, kiosk_check_in).

% Associations define relationships between actors and use cases
association(tour_guide, group_check_in).
association(passenger, individual_check_in).
association(passenger, security_screening).

% -------------------------------
% 5. VALIDATION PREDICATES
% -------------------------------

% Validation predicates are used to ensure that the relations are defined correctly
% Generalizations, includes, extends, and associations are checked to confirm that both entities involved are properly defined
valid_generalization(X, Y) :- (actor(X); use_case(X)), (actor(Y); use_case(Y)).
valid_include(From, To) :- use_case(From), use_case(To).
valid_extend(From, To) :- use_case(From), use_case(To).
valid_association(Actor, UC) :- actor(Actor), use_case(UC).

% -------------------------------
% 6. COUNT USE CASES IN A PACKAGE
% -------------------------------

% This predicate counts the number of use cases in a specific package
count_use_cases(Package, Count) :-
    findall(UC, use_case_in_package(UC, Package), List),
    length(List, Count).

% -------------------------------
% 7. PLANTUML GENERATION
% -------------------------------

% Write a line of text to the output
write_line(Line) :- write(Line), nl.

% Convert atoms (like 'individual_check_in') into a human-readable label format
% This function takes care of formatting labels for the PlantUML diagram
atom_concat_words(Atom, Label) :-
    atomic_list_concat(Words, '_', Atom),  % Split the atom by underscores
    maplist(capitalize, Words, CapWords),  % Capitalize each word
    atomic_list_concat(CapWords, ' ', Label).  % Join with a single space

% Capitalize the first letter of each word
capitalize(Word, Capitalized) :-
    atom_chars(Word, [H|T]),
    upcase_atom(H, UH),
    atom_chars(Capitalized, [UH|T]).

% Reformat the label if it contains multiple words
% This function ensures that multi-word labels are formatted in a readable way for PlantUML
reformat_label(In, Out) :-
    split_string(In, " ", "", Words),
    length(Words, Len),
    ( Len > 1 ->
        append(Front, [Last], Words),
        atomic_list_concat(Front, ' ', BeforeLast),
        format(string(Out), "~w\\\\n~w", [BeforeLast, Last])
    ; Out = In
    ).

% Generate actor definitions for PlantUML
% This generates a "actor" line for each actor in the system
generate_actors :-
    forall(actor(A), (
        alias(A, Alias),
        atom_concat_words(A, Label),
        format('actor "~w" as ~w~n', [Label, Alias])
    )).

% Generate use case definitions within a rectangle
% This generates a "usecase" line for each use case in the system, grouped in a business package
generate_use_cases :-
    write_line('rectangle "<<Business>>\\\\nAirport" {'),
    forall(use_case(U), (
        alias(U, Alias),
        atom_concat_words(U, Label),
        format('  usecase "~w" as ~w~n', [Label, Alias])
    )),
    write_line('}').

% Generate relationships between actors, use cases, and other relationships
% This handles generalizations, includes, extends, and associations, translating them into PlantUML format
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

% Save the PlantUML code to a file
save_plantuml_to_file(FileName) :-
    open(FileName, write, Stream),
    with_output_to(Stream, (
        write_line('@startuml'),
        write_line('left to right direction'),
        write_line('skinparam packageStyle rectangle'),
        generate_actors,
        generate_use_cases,
        generate_relationships,
        write_line('center footer uml-diagrams.org'),
        write_line('@enduml')
    )),
    close(Stream).

% Main function to generate the entire PlantUML diagram
% This writes the initial PlantUML code, including the layout direction, package style, actors, use cases, and relationships
generate_plantuml :-
    count_use_cases(check_in, Count),
    format('Number of use cases: ~w~n', [Count]),
    write_line('@startuml'),
    write_line('left to right direction'),
    write_line('skinparam packageStyle rectangle'),
    generate_actors,
    generate_use_cases,
    generate_relationships,
    write_line('center footer uml-diagrams.org'),
    write_line('@enduml'),
    save_plantuml_to_file('diagram.puml').
