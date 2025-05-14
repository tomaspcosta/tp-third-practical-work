% -------------------------------
% LOAD THE INPUT FILE
% -------------------------------
% Read the input file dynamically
:- [input].

% -------------------------------
% VALIDATION PREDICATES
% -------------------------------
% Ensure that the relations are defined correctly
valid_generalization(X, Y) :- (actor(X); use_case(X)), (actor(Y); use_case(Y)).
valid_include(From, To) :- use_case(From), use_case(To).
valid_extend(From, To) :- use_case(From), use_case(To).
valid_association(Actor, UC) :- actor(Actor), use_case(UC).

% -------------------------------
% PLANTUML GENERATION
% -------------------------------

% Write a line of text to the output
write_line(Line) :- write(Line), nl.

% Capitalize the first letter of each word
capitalize(Word, Capitalized) :-
    atom_chars(Word, [H|T]),
    upcase_atom(H, UH),
    atom_chars(Capitalized, [UH|T]).

% Convert atoms (like 'individual_check_in') into a human-readable label format
atom_concat_words(Atom, Label) :-
    atomic_list_concat(Words, '_', Atom),  % Split the atom by underscores
    maplist(capitalize, Words, CapWords),  % Capitalize each word
    atomic_list_concat(CapWords, ' ', Label).  % Join with a single space

% -------------------------------
% COUNT USE CASES
% -------------------------------
% Predicate to count all use cases in the input
count_use_cases(Count) :-
    findall(U, use_case(U), UseCases),
    length(UseCases, Count).

% -------------------------------
% 4. GENERATE PLANTUML OUTPUT
% -------------------------------

% Generate actor definitions for PlantUML
generate_actors :-
    forall(actor(A), (
        atom_concat_words(A, Label),
        format('actor "~w" as ~w~n', [Label, A])
    )).

% Generate use case definitions within a rectangle
generate_use_cases :-
    write_line('rectangle "<<Business>>\\\\nAirport" {'),
    forall(use_case(U), (
        atom_concat_words(U, Label),
        format('  usecase "~w" as ~w~n', [Label, U])
    )),
    write_line('}').

% Generate relationships between actors, use cases, and other relationships
generate_relationships :-
    forall(generalization(Sub, Super), (
        format('~w --|> ~w~n', [Sub, Super])
    )),
    forall(include(From, To), (
        format('~w ..> ~w : <<include>>~n', [From, To])
    )),
    forall(extend(From, To), (
        format('~w ..> ~w : <<extend>>~n', [From, To])
    )),
    forall(association(A, UC), (
        format('~w -- ~w~n', [A, UC])
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

% Main function to generate the entire PlantUML diagram and display the count of use cases
generate_plantuml :-
    write_line('@startuml'),
    write_line('left to right direction'),
    write_line('skinparam packageStyle rectangle'),
    generate_actors,
    generate_use_cases,
    generate_relationships,
    write_line('center footer uml-diagrams.org'),
    write_line('@enduml'),
    save_plantuml_to_file('diagram.puml'),

    % Count and display the number of use cases
    count_use_cases(Count),
    format('Number of use cases: ~w~n', [Count]).  % Display the count to the console
