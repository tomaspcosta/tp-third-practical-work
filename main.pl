:- use_module(library(readutil)). % For reading user input
:- use_module(library(lists)).    % For list utilities

% Dynamic predicates for storing diagram data.
:- dynamic actor/1,
           use_case/1,
           association/2,          % actor_name, use_case_name
           include/2,              % base_uc_name, included_uc_name
           extend/2,               % extending_uc_name, base_uc_name
           generalization/2,       % child_uc_name, parent_uc_name
           actor_generalization/2, % child_actor_name, parent_actor_name
           system/1.               % The main system/package name

% -------------------------------
% Entry Point & Initial Menu
% -------------------------------

start :-
    choose_mode.

% Displays the initial menu and prompts for user action.
choose_mode :-
    nl,
    write('=== Initial Menu ==='), nl,
    write('1. Create a new diagram'), nl,
    write('0. Exit Program'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        handle_initial_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, choose_mode
    ).

% Handles the users selection from the initial menu.
handle_initial_choice(1) :-
    clear_data,
    action_set_system_name,
    main_menu.
handle_initial_choice(0) :-
    action_exit_program.
handle_initial_choice(_) :-
    write('Invalid option. Please try again.'), nl,
    choose_mode.

% Prompts for and sets the system/package name.
action_set_system_name :-
    nl,
    write('Enter system name for the diagram (this will act as the main package): '),
    read_line_to_string(user_input, SystemName),
    ( SystemName == "" ->
        write('System name cannot be empty. Please try again.'), nl,
        action_set_system_name
    ; (retract(system(_OldName)) -> true ; true),
      assertz(system(SystemName)),
      format('System (main package) set to "~w".~n', [SystemName])
    ).

% Removes all stored data, resetting the diagram.
clear_data :-
    retractall(actor(_)),
    retractall(use_case(_)),
    retractall(association(_, _)),
    retractall(include(_, _)),
    retractall(extend(_, _)),
    retractall(generalization(_, _)),
    retractall(actor_generalization(_, _)),
    retractall(system(_)).

% ==================================================
% Main Menu
% ==================================================

% Displays the main menu for managing elements, relationships, viewing, exporting, etc.
main_menu :-
    nl,
    ( system(SName) -> format('Current System/Package: ~w~n', [SName]) ; write('No system/package defined yet. Consider setting one (Option 7).~n')), % Note: Option number for system name changed
    write('=== Main Menu ==='), nl,
    write('1. Manage Elements (Actors, Use Cases)'), nl,
    write('2. Manage Relationships'), nl,
    write('3. Remove Elements/Relationships'), nl,
    write('4. View Diagram Details'), nl,
    write('5. Generate .puml Diagram File'), nl,
    write('6. Show Use Case Count'), nl,
    write('7. Change/Define System Name'), nl,
    write('8. Return to Initial Menu (clears current diagram)'), nl,
    write('0. Exit Program'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Option) ->
        handle_main_menu_choice(Option)
    ; write('Invalid input. Please enter a number.'), nl, main_menu
    ).

% Handles user selection from the main menu.
handle_main_menu_choice(1) :- manage_elements_menu, main_menu.
handle_main_menu_choice(2) :- manage_relationships_menu, main_menu.
handle_main_menu_choice(3) :- manage_remove_menu, main_menu.
handle_main_menu_choice(4) :- view_diagram_details_top_menu, main_menu.
handle_main_menu_choice(5) :- action_generate_puml_file, main_menu.
handle_main_menu_choice(6) :- count_use_cases_in_system_detail, main_menu.
handle_main_menu_choice(7) :- action_set_system_name, main_menu.
handle_main_menu_choice(8) :- action_return_to_initial_menu.
handle_main_menu_choice(0) :- action_exit_program.
handle_main_menu_choice(_) :- write('Invalid option number. Please try again.'), nl, main_menu.

% Clears all data and returns to the initial menu.
action_return_to_initial_menu :-
    write('Clearing current diagram and returning to initial menu...'), nl,
    clear_data,
    choose_mode.

% Exits the program.
action_exit_program :-
    write('Goodbye!'), nl.

% ==================================================
% 1. Manage Elements Menu & Actions
% ==================================================
% Menu for adding actors or use cases.
manage_elements_menu :-
    nl,
    write('--- Manage Elements ---'), nl,
    write('1. Add Actor'), nl,
    write('2. Add Use Case'), nl,
    write('0. Back to Main Menu'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_manage_elements_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, manage_elements_menu
    ).

% Handles selection for adding an actor or use case.
process_manage_elements_choice(1) :- action_add_actor, manage_elements_menu.
process_manage_elements_choice(2) :- action_add_use_case, manage_elements_menu.
process_manage_elements_choice(0).
process_manage_elements_choice(_) :- write('Invalid option. Please try again.'), nl, manage_elements_menu.

% Adds a new actor to the diagram.
action_add_actor :-
    write('Enter actor name (or 0 to cancel): '),
    read_line_to_string(user_input, ActorInput),
    ( ActorInput == "0" -> true
    ; ActorInput == "" -> write('Actor name cannot be empty.'), nl
    ; actor(ActorInput) -> write('Actor already exists.'), nl
    ; assertz(actor(ActorInput)), format('Actor added: ~w~n', [ActorInput])
    ).

% Adds a new use case to the diagram.
action_add_use_case :-
    write('Enter use case name (or 0 to cancel): '),
    read_line_to_string(user_input, UseCaseInput),
    ( UseCaseInput == "0" -> true
    ; UseCaseInput == "" -> write('Use case name cannot be empty.'), nl
    ; use_case(UseCaseInput) -> write('Use case already exists.'), nl
    ; assertz(use_case(UseCaseInput)), format('Use case added: ~w~n', [UseCaseInput])
    ).

% ==================================================
% 2. Manage Relationships Menu & Actions
% ==================================================
% Menu for adding associations, includes, extends, and generalizations.
manage_relationships_menu :-
    nl,
    write('--- Manage Relationships ---'), nl,
    write('1. Add Association (Actor -- Use Case)'), nl,
    write('2. Add <<include>> Relation (Use Case ..> Use Case)'), nl,
    write('3. Add <<extend>> Relation (Use Case ..> Use Case)'), nl,
    write('4. Add Generalization (Actor or Use Case)'), nl,
    write('0. Back to Main Menu'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_manage_relationships_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, manage_relationships_menu
    ).

% Handles selection for relationship management.
process_manage_relationships_choice(1) :- action_add_association, manage_relationships_menu.
process_manage_relationships_choice(2) :- action_add_include, manage_relationships_menu.
process_manage_relationships_choice(3) :- action_add_extend, manage_relationships_menu.
process_manage_relationships_choice(4) :- action_manage_generalizations, manage_relationships_menu.
process_manage_relationships_choice(0).
process_manage_relationships_choice(_) :- write('Invalid option. Please try again.'), nl, manage_relationships_menu.

% Adds an association between an actor and a use case.
action_add_association :-
    ( list_actors_or_prompt_add ->
        write('Choose actor by number for association (or 0 to cancel): '),
        read_line_to_string(user_input, AI),
        ( AI == "0" -> true
        ; atom_number(AI, AIndex), get_nth_actor(AIndex, Actor) ->
            ( list_use_cases_or_prompt_add ->
                write('Choose use case by number for association (or 0 to cancel): '),
                read_line_to_string(user_input, UI),
                ( UI == "0" -> true
                ; atom_number(UI, UIndex), get_nth_use_case(UIndex, UseCase) ->
                    ( association(Actor, UseCase) -> write('This association already exists.'), nl
                    ; assertz(association(Actor, UseCase)),
                      format('Association added: Actor "~w" -- Use Case "~w"~n', [Actor, UseCase])
                    )
                ; write('Invalid use case selection.'), nl
                )
            ; true
            )
        ; write('Invalid actor selection.'), nl
        )
    ; true
    ).

% Adds an include relationship between use cases.
action_add_include :-
    ( list_use_cases_or_prompt_add(2) ->
        write('Choose base use case (FROM which includes) (number) (or 0 to cancel): '),
        read_line_to_string(user_input, BaseUI),
        ( BaseUI == "0" -> true
        ; atom_number(BaseUI, BaseIndex), get_nth_use_case(BaseIndex, BaseUseCase) ->
            write('Choose use case to be included (TO) (number) (or 0 to cancel): '),
            read_line_to_string(user_input, InclUI),
            ( InclUI == "0" -> true
            ; atom_number(InclUI, InclIndex), get_nth_use_case(InclIndex, IncludedUseCase) ->
                ( BaseUseCase == IncludedUseCase -> write('A use case cannot include itself.'), nl
                ; include(BaseUseCase, IncludedUseCase) -> write('This <<include>> relation already exists.'), nl
                ; assertz(include(BaseUseCase, IncludedUseCase)),
                  format('Relation added: "~w" ..> "~w" : <<include>>~n', [BaseUseCase, IncludedUseCase])
                )
            ; write('Invalid "TO" use case selection.'), nl
            )
        ; write('Invalid "FROM" use case selection.'), nl
        )
    ; true
    ).

% Adds an extend relationship between use cases.
action_add_extend :-
    ( list_use_cases_or_prompt_add(2) ->
        write('Choose extending use case (FROM which extends) (number) (or 0 to cancel): '),
        read_line_to_string(user_input, ExtUI),
        ( ExtUI == "0" -> true
        ; atom_number(ExtUI, ExtIndex), get_nth_use_case(ExtIndex, ExtendingUseCase) ->
            write('Choose base use case to be extended (TO) (number) (or 0 to cancel): '),
            read_line_to_string(user_input, BaseUI),
            ( BaseUI == "0" -> true
            ; atom_number(BaseUI, BaseIndex), get_nth_use_case(BaseIndex, BaseUseCase) ->
                ( ExtendingUseCase == BaseUseCase -> write('A use case cannot extend itself.'), nl
                ; extend(ExtendingUseCase, BaseUseCase) -> write('This <<extend>> relation already exists.'), nl
                ; assertz(extend(ExtendingUseCase, BaseUseCase)),
                  format('Relation added: "~w" ..> "~w" : <<extend>>~n', [ExtendingUseCase, BaseUseCase])
                )
            ; write('Invalid "TO" use case selection.'), nl
            )
        ; write('Invalid "FROM" use case selection.'), nl
        )
    ; true
    ).

% Menu for adding generalizations between actors or use cases.
action_manage_generalizations :-
    nl,
    write('--- Add Generalization ---'), nl,
    write('1. Actor Generalization'), nl,
    write('2. Use Case Generalization'), nl,
    write('0. Back to Relationships Menu'), nl,
    write('Choose type of generalization: '),
    read_line_to_string(user_input, GenTypeInput),
    ( atom_number(GenTypeInput, GenTypeChoice) ->
        process_generalization_type_choice(GenTypeChoice)
    ; write('Invalid input. Please enter a number.'), nl, action_manage_generalizations
    ).

% Handles the type of generalization to add.
process_generalization_type_choice(1) :- add_actor_generalization_interaction, action_manage_generalizations.
process_generalization_type_choice(2) :- add_use_case_generalization_interaction, action_manage_generalizations.
process_generalization_type_choice(0).
process_generalization_type_choice(_) :- write('Invalid generalization type choice.'), nl, action_manage_generalizations.

% Adds a generalization between two actors.
add_actor_generalization_interaction :-
    ( list_actors_or_prompt_add(2) ->
        write('Choose child actor (specific type) (number) (or 0 to cancel): '),
        read_line_to_string(user_input, CI),
        ( CI == "0" -> !
        ; atom_number(CI, CIndex), get_nth_actor(CIndex, ChildActor) ->
            write('Choose parent actor (general type) (number) (or 0 to cancel): '),
            read_line_to_string(user_input, PI),
            ( PI == "0" -> !
            ; atom_number(PI, PIndex), get_nth_actor(PIndex, ParentActor) ->
                ( ChildActor == ParentActor -> write('Child and parent actor cannot be the same.'), nl
                ; actor_generalization(ChildActor, ParentActor) -> write('This actor generalization already exists.'), nl
                ; assertz(actor_generalization(ChildActor, ParentActor)),
                  format('Actor generalization added: "~w" --|> "~w"~n', [ChildActor, ParentActor])
                )
            ; write('Invalid parent actor selection.'), nl
            )
        ; write('Invalid child actor selection.'), nl
        )
    ; !
    ).

% Adds a generalization between two use cases.
add_use_case_generalization_interaction :-
    ( list_use_cases_or_prompt_add(2) ->
        write('Choose child use case (specific type) (number) (or 0 to cancel): '),
        read_line_to_string(user_input, CI),
        ( CI == "0" -> !
        ; atom_number(CI, CIndex), get_nth_use_case(CIndex, ChildUseCase) ->
            write('Choose parent use case (general type) (number) (or 0 to cancel): '),
            read_line_to_string(user_input, PI),
            ( PI == "0" -> !
            ; atom_number(PI, PIndex), get_nth_use_case(PIndex, ParentUseCase) ->
                ( ChildUseCase == ParentUseCase -> write('Child and parent use case cannot be the same.'), nl
                ; generalization(ChildUseCase, ParentUseCase) -> write('This use case generalization already exists.'), nl
                ; assertz(generalization(ChildUseCase, ParentUseCase)),
                  format('Use case generalization added: "~w" --|> "~w"~n', [ChildUseCase, ParentUseCase])
                )
            ; write('Invalid parent use case selection.'), nl
            )
        ; write('Invalid child use case selection.'), nl
        )
    ; !
    ).

% ==================================================
% 3. View Diagram Details Menu & Actions
% ==================================================
% This menu lets you see everything or just specific details about your diagram.
view_diagram_details_top_menu :-
    nl,
    write('--- View Diagram Details ---'), nl,
    write('1. List Everything'), nl,
    write('2. List Specific Details'), nl,
    write('0. Back to Main Menu'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_view_details_top_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, view_diagram_details_top_menu
    ).

% This part handles what you want to see in the details menu.
process_view_details_top_choice(1) :- list_everything_detail, view_diagram_details_top_menu.
process_view_details_top_choice(2) :- view_specific_details_menu, view_diagram_details_top_menu.
process_view_details_top_choice(0).
process_view_details_top_choice(_) :- write('Invalid choice.'), nl, view_diagram_details_top_menu.

% This menu lets you pick exactly what you want to list (actors, use cases, etc).
view_specific_details_menu :-
    nl,
    write('--- List Specific Details ---'), nl,
    write('1. List All Actors'), nl,
    write('2. List All Use Cases'), nl,
    write('3. List Associations (Actor -- Use Case)'), nl,
    write('4. List <<include>> Relations'), nl,
    write('5. List <<extend>> Relations'), nl,
    write('6. List Actor Generalizations (Actor --|> Actor)'), nl,
    write('7. List Use Case Generalizations (Use Case --|> Use Case)'), nl,
    write('0. Back to View Diagram Details Menu'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_specific_details_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, view_specific_details_menu
    ).

% This part handles which specific list you want to see.
process_specific_details_choice(1) :- list_all_actors_detail, view_specific_details_menu.
process_specific_details_choice(2) :- list_all_use_cases_detail, view_specific_details_menu.
process_specific_details_choice(3) :- list_all_associations_detail, view_specific_details_menu.
process_specific_details_choice(4) :- list_all_includes_detail, view_specific_details_menu.
process_specific_details_choice(5) :- list_all_extends_detail, view_specific_details_menu.
process_specific_details_choice(6) :- list_all_actor_generalizations_detail, view_specific_details_menu.
process_specific_details_choice(7) :- list_all_uc_generalizations_detail, view_specific_details_menu.
process_specific_details_choice(0).
process_specific_details_choice(_) :- write('Invalid choice.'), nl, view_specific_details_menu.

% This function prints all the actors you have.
list_all_actors_detail :-
    nl, write('--- Actors ---'), nl,
    findall(Actor, actor(Actor), Actors),
    ( Actors == [] -> write('No actors defined.')
    ; print_list_indexed(Actors, 1)
    ), nl.

% This one prints all the use cases.
list_all_use_cases_detail :-
    nl, write('--- Use Cases ---'), nl,
    findall(UseCase, use_case(UseCase), UseCases),
    ( UseCases == [] -> write('No use cases defined.')
    ; print_list_indexed(UseCases, 1)
    ), nl.

% This shows how many use cases are in your system.
count_use_cases_in_system_detail :-
    nl, write('--- Count of Use Cases in System/Package ---'), nl,
    ( system(SystemName) ->
        findall(UseCase, use_case(UseCase), UseCases),
        length(UseCases, Count),
        format('The system/package "~w" contains ~d use case(s).~n', [SystemName, Count])
    ; write('No system/package defined. Cannot count use cases.'), nl
    ), nl.

% list_all_associations_detail0: Lists all actor usecase associations
list_all_associations_detail :-
    nl, write('--- Associations (Actor -- Use Case) ---'), nl,
    findall(association(Actor, UC), association(Actor, UC), Associations),
    ( Associations == [] -> write('No associations defined.')
    ; print_relationship_list_indexed(Associations, 1, '--')
    ), nl.

% list_all_includes_detail/0: Lists all include relationships.
list_all_includes_detail :-
    nl, write('--- <<include>> Relations (Base UC ..> Included UC) ---'), nl,
    findall(include(Base, Incl), include(Base, Incl), Includes),
    ( Includes == [] -> write('No <<include>> relations defined.')
    ; print_relationship_list_indexed(Includes, 1, '..> <<include>>')
    ), nl.

% list_all_extends_detail/0: Lists all extend relationships.
list_all_extends_detail :-
    nl, write('--- <<extend>> Relations (Extending UC ..> Base UC) ---'), nl,
    findall(extend(Extending, Base), extend(Extending, Base), Extends),
    ( Extends == [] -> write('No <<extend>> relations defined.')
    ; print_relationship_list_indexed(Extends, 1, '..> <<extend>>')
    ), nl.

% list_all_actor_generalizations_detail/0: Lists all actor generalizations.
list_all_actor_generalizations_detail :-
    nl, write('--- Actor Generalizations (Child Actor --|> Parent Actor) ---'), nl,
    findall(actor_generalization(Child, Parent), actor_generalization(Child, Parent), ActorGens),
    ( ActorGens == [] -> write('No actor generalizations defined.')
    ; print_relationship_list_indexed(ActorGens, 1, '--|>')
    ), nl.

% list_all_uc_generalizations_detail/0: Lists all use case generalizations.
list_all_uc_generalizations_detail :-
    nl, write('--- Use Case Generalizations (Child UC --|> Parent UC) ---'), nl,
    findall(generalization(Child, Parent), generalization(Child, Parent), UCGens),
    ( UCGens == [] -> write('No use case generalizations defined.')
    ; print_relationship_list_indexed(UCGens, 1, '--|>')
    ), nl.

% list_everything_detail/0: Lists all elements and relationships in the diagram.
list_everything_detail :-
    nl, write('============================='), nl,
    write('=== Full Diagram Summary ===='), nl,
    write('============================='), nl,
    (system(SName) -> format('System/Package: ~w~n', [SName]) ; write('System/Package: Not defined~n')), nl,
    list_all_actors_detail,
    list_all_use_cases_detail,
    count_use_cases_in_system_detail,
    list_all_associations_detail,
    list_all_includes_detail,
    list_all_extends_detail,
    list_all_actor_generalizations_detail,
    list_all_uc_generalizations_detail,
    write('===== End of Summary ====='), nl.


% ==================================================
% 7. Remove Elements/Relationships Menu & Actions
% ==================================================
manage_remove_menu :-
    nl,
    write('--- Remove Elements/Relationships ---'), nl,
    write('1. Remove Actor (and related connections)'), nl,
    write('2. Remove Use Case (and related connections)'), nl,
    write('3. Remove Association'), nl,
    write('4. Remove <<include>> Relation'), nl,
    write('5. Remove <<extend>> Relation'), nl,
    write('6. Remove Actor Generalization'), nl,
    write('7. Remove Use Case Generalization'), nl,
    write('0. Back to Main Menu'), nl,
    write('Choose an option: '),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_remove_menu_choice(Choice)
    ; write('Invalid input. Please enter a number.'), nl, manage_remove_menu
    ).

process_remove_menu_choice(1) :- action_remove_actor, manage_remove_menu.
process_remove_menu_choice(2) :- action_remove_use_case, manage_remove_menu.
process_remove_menu_choice(3) :- action_remove_association, manage_remove_menu.
process_remove_menu_choice(4) :- action_remove_include, manage_remove_menu.
process_remove_menu_choice(5) :- action_remove_extend, manage_remove_menu.
process_remove_menu_choice(6) :- action_remove_actor_generalization, manage_remove_menu.
process_remove_menu_choice(7) :- action_remove_uc_generalization, manage_remove_menu.
process_remove_menu_choice(0).
process_remove_menu_choice(_) :- write('Invalid option. Please try again.'), nl, manage_remove_menu.

action_remove_actor :-
    nl, write('--- Remove Actor ---'), nl,
    findall(Actor, actor(Actor), Actors),
    ( Actors == [] -> write('No actors to remove.'), nl
    ; write('Actors:'), nl,
      print_list_indexed(Actors, 1),
      write('Enter number of actor to remove (or 0 to cancel): '),
      read_line_to_string(user_input, IndexStr),
      ( IndexStr == "0" -> write('Cancelled.'), nl
      ; atom_number(IndexStr, Index), get_nth_item(actor, Index, ActorToRemove) ->
          retract(actor(ActorToRemove)),
          retractall(association(ActorToRemove, _)),
          retractall(association(_, ActorToRemove)),
          retractall(actor_generalization(ActorToRemove, _)),
          retractall(actor_generalization(_, ActorToRemove)),
          format('Actor "~w" and all its connections removed.~n', [ActorToRemove])
      ; write('Invalid actor selection or actor not found.'), nl
      )
    ).

action_remove_use_case :-
    nl, write('--- Remove Use Case ---'), nl,
    findall(UC, use_case(UC), UseCases),
    ( UseCases == [] -> write('No use cases to remove.'), nl
    ; write('Use Cases:'), nl,
      print_list_indexed(UseCases, 1),
      write('Enter number of use case to remove (or 0 to cancel): '),
      read_line_to_string(user_input, IndexStr),
      ( IndexStr == "0" -> write('Cancelled.'), nl
      ; atom_number(IndexStr, Index), get_nth_item(use_case, Index, UCToRemove) ->
          retract(use_case(UCToRemove)),
          retractall(association(_, UCToRemove)),
          retractall(include(UCToRemove, _)),
          retractall(include(_, UCToRemove)),
          retractall(extend(UCToRemove, _)),
          retractall(extend(_, UCToRemove)),
          retractall(generalization(UCToRemove, _)),
          retractall(generalization(_, UCToRemove)),
          format('Use Case "~w" and all its connections removed.~n', [UCToRemove])
      ; write('Invalid use case selection or use case not found.'), nl
      )
    ).

action_remove_association :-
    remove_relationship_interactive(association, 'Association', '--').
action_remove_include :-
    remove_relationship_interactive(include, '<<include>> Relation', '..> <<include>>').
action_remove_extend :-
    remove_relationship_interactive(extend, '<<extend>> Relation', '..> <<extend>>').
action_remove_actor_generalization :-
    remove_relationship_interactive(actor_generalization, 'Actor Generalization', '--|>').
action_remove_uc_generalization :-
    remove_relationship_interactive(generalization, 'Use Case Generalization', '--|>').

% Generic helper to remove a specific relationship
remove_relationship_interactive(Functor, DisplayName, Separator) :-
    nl, format('--- Remove ~w ---~n', [DisplayName]),
    Goal =.. [Functor, _Arg1, _Arg2],
    findall(Goal, Goal, Relationships),
    ( Relationships == [] -> format('No ~ws to remove.~n', [DisplayName]), nl
    ; format('~ws:~n', [DisplayName]),
      print_relationship_list_indexed(Relationships, 1, Separator),
      write('Enter number of relationship to remove (or 0 to cancel): '),
      read_line_to_string(user_input, IndexStr),
      ( IndexStr == "0" -> write('Cancelled.'), nl
      ; atom_number(IndexStr, Index), nth1(Index, Relationships, RelToRemove) ->
          retract(RelToRemove),
          RelToRemove =.. [_, RelArg1, RelArg2],
          format('~w: "~w" ~w "~w" removed.~n', [DisplayName, RelArg1, Separator, RelArg2])
      ; write('Invalid selection or relationship not found.'), nl
      )
    ).

% action_generate_puml_file/0: Prompts for filename and generates the PlantUML file.
action_generate_puml_file :-
    ( \+ system(_) ->
        write('No system/package defined. Please define a system name first (Main Menu option 6).'), nl
    ; system(SystemName),
      format('The default filename will be "~w.puml".~n', [SystemName]),
      write('Enter a custom filename (without .puml extension) or press Enter to use the default (or 0 to cancel): '),
      read_line_to_string(user_input, CustomNameInput),
      ( CustomNameInput == "0" -> true
      ; CustomNameInput == "" -> generate_puml_file(SystemName)
      ; generate_puml_file(CustomNameInput)
      )
    ).

% -------------------------------
% Helpers (Listing, Nth item, Sanitization)
% -------------------------------

% list_actors_or_prompt_add/0, list_use_cases_or_prompt_add/0: Ensures enough actors/use cases exist for selection.
list_actors_or_prompt_add :- list_items_or_prompt_add(actor, 'actors', 'actor', 1).
list_actors_or_prompt_add(MinCount) :- list_items_or_prompt_add(actor, 'actors', 'actor', MinCount).
list_use_cases_or_prompt_add :- list_items_or_prompt_add(use_case, 'use cases', 'use case', 1).
list_use_cases_or_prompt_add(MinCount) :- list_items_or_prompt_add(use_case, 'use cases', 'use case', MinCount).

% list_items_or_prompt_add/4: Lists items of a given type, or prompts to add more if not enough.
list_items_or_prompt_add(TypeAtom, PluralName, _SingularName, MinCount) :-
    Goal =.. [TypeAtom, ItemNameVariable],
    findall(ItemNameVariable, Goal, ItemsList),
    length(ItemsList, Count),
    ( Count < MinCount ->
        format('Not enough ~w defined (need at least ~d, found ~d). Please add more ~w first from "Manage Elements" menu.~n', [PluralName, MinCount, Count, PluralName]),
        fail
    ; write(PluralName), write(':'), nl,
      print_list_indexed(ItemsList, 1),
      true
    ).

% print_list_indexed/2: Prints a list with indices.
print_list_indexed([], _).
print_list_indexed([H|T], N) :-
    format('~d. ~w~n', [N, H]),
    N1 is N + 1,
    print_list_indexed(T, N1).

% Helper to print relationship lists
print_relationship_list_indexed([], _, _).
print_relationship_list_indexed([Rel|T], N, Separator) :-
    Rel =.. [_Functor, Arg1Rel, Arg2Rel],
    format('~d. ~w ~w ~w~n', [N, Arg1Rel, Separator, Arg2Rel]),
    N1 is N + 1,
    print_relationship_list_indexed(T, N1, Separator).



get_nth_actor(N, Actor) :- get_nth_item(actor, N, Actor).
get_nth_use_case(N, UseCase) :- get_nth_item(use_case, N, UseCase).

% get_nth_item/3: Retrieves the N-th item of a given type (for single elements).
get_nth_item(TypeAtom, N, ChosenItem) :-
    Goal =.. [TypeAtom, ItemNameVariable],
    findall(ItemNameVariable, Goal, ItemsList),
    ( N > 0, length(ItemsList, Len), N =< Len ->
        nth1(N, ItemsList, ChosenItem)
    ; !, fail
    ).

% safe_name/2: Converts a name to a PlantUML-safe identifier.
safe_name(InputName, SafeID) :-
    (   string(InputName) -> StringForProcessing = InputName
    ;   atom(InputName)   -> atom_string(InputName, StringForProcessing)
    ;   number(InputName) -> number_string(InputName, StringForProcessing)
    ;   compound(InputName) -> term_string(InputName, StringForProcessing)
    ;   var(InputName)      -> StringForProcessing = "unbound_variable_id"
    ;   format(string(StringForProcessing), "~w", [InputName])
    ),
    string_lower(StringForProcessing, LowercaseString),
    replace_problematic_chars(LowercaseString, UnderscoredString),
    remove_non_alnum_underscore(UnderscoredString, CleanedString),
    ensure_valid_plantuml_id(CleanedString, LowercaseString, SafeID).

% replace_problematic_chars/2: Replaces spaces, dashes, tabs, and newlines with underscores.
replace_problematic_chars(In, Out) :-
    replace_chars(In, " ", "_", Temp1),
    replace_chars(Temp1, "-", "_", Temp2),
    replace_chars(Temp2, "\t", "_", Temp3),
    replace_chars(Temp3, "\n", "_", Out).

% replace_chars/4: Helper for replacing characters in a string.
replace_chars(Input, ToReplace, Replacement, Output) :-
    split_string(Input, ToReplace, ToReplace, Parts),
    atomic_list_concat(Parts, Replacement, Output).

% remove_non_alnum_underscore/2: Removes all characters except alphanumeric and underscore.
remove_non_alnum_underscore(InputString, OutputString) :-
    string_chars(InputString, Chars),
    include(is_plantuml_safe_char, Chars, SafeChars),
    string_chars(OutputString, SafeChars).

is_plantuml_safe_char(Char) :-
    char_type(Char, alnum) ; Char == '_'.

% ensure_valid_plantuml_id/3: Ensures the identifier is valid for PlantUML.
ensure_valid_plantuml_id(CleanedString, OriginalLowerString, SafeID) :-
    ( CleanedString == "" ->
        string_chars(OriginalLowerString, OriginalChars),
        include(char_type(alnum), OriginalChars, AlnumOnlyChars),
        ( AlnumOnlyChars == [] -> SafeID = "unspecified_id"
        ; string_chars(FallbackIDBase, AlnumOnlyChars),
          atom_concat('id_', FallbackIDBase, SafeID)
        )
    ; sub_string(CleanedString, 0, 1, _, FirstChar),
      ( char_type(FirstChar, digit) ->
          atom_concat('id_', CleanedString, SafeID)
      ; SafeID = CleanedString
      )
    ).

% -------------------------------
% Generate .puml file
% -------------------------------

% generate_puml_file/1: Generates a PlantUML file with the current diagram.
generate_puml_file(BaseFileNameAtomOrString) :-
    ( atom(BaseFileNameAtomOrString) -> BaseFileName = BaseFileNameAtomOrString
    ; string(BaseFileNameAtomOrString) -> atom_string(BaseFileName, BaseFileNameAtomOrString)
    ; format(atom(BaseFileName), '~w', [BaseFileNameAtomOrString])
    ),
    ( \+ system(_) ->
        write('Error: System name not defined. Cannot generate file.'), nl
    ; format(atom(FileName), '~w.puml', [BaseFileName]),
      open(FileName, write, Stream),
      write_puml_content(Stream),
      close(Stream),
      (system(SysName) -> DiagramName = SysName ; DiagramName = BaseFileName),
      format('Diagram "~w" saved to file "~w"~n', [DiagramName, FileName])
    ).

% write_puml_content/1: Writes the PlantUML content to the given stream.
write_puml_content(Stream) :-
    write(Stream, '@startuml'), nl(Stream),
    ( system(SysName) -> format(Stream, "title Use Case Diagram for ~w~n~n", [SysName])
    ; format(Stream, "title Use Case Diagram~n~n", [])
    ),
    write(Stream, "left to right direction"), nl(Stream),
    write(Stream, "skinparam packageStyle rectangle"), nl(Stream),
    nl(Stream),
    write_actors_puml(Stream), nl(Stream),
    write_system_boundary_puml(Stream), nl(Stream),
    write_associations_puml(Stream), nl(Stream),
    write_includes_puml(Stream), nl(Stream),
    write_extends_puml(Stream), nl(Stream),
    write_uc_generalizations_puml(Stream), nl(Stream),
    write_actor_generalizations_puml(Stream), nl(Stream),
    write(Stream, "@enduml"), nl(Stream).

% write_actors_puml/1: Writes all actors in PlantUML format.
write_actors_puml(Stream) :-
    forall(actor(A), ( safe_name(A, SafeA), format(Stream, 'actor "~w" as ~w~n', [A, SafeA]) )).

% write_system_boundary_puml/1: Writes the system/package boundary and use cases.
write_system_boundary_puml(Stream) :-
    ( system(SystemName) ->
        safe_name(SystemName, SafeSystemNameId),
        format(Stream, 'rectangle "<<System>>\\n~w" as ~w_boundary {~n', [SystemName, SafeSystemNameId]),
        forall(use_case(U), ( safe_name(U, SafeU), format(Stream, '  usecase "~w" as ~w~n', [U, SafeU]) )),
        write(Stream, '}'), nl(Stream)
    ; forall(use_case(U), ( safe_name(U, SafeU), format(Stream, 'usecase "~w" as ~w~n', [U, SafeU]) ))
    ).

% write_associations_puml/1: Writes all actor-use case associations.
write_associations_puml(Stream) :-
    forall(association(Actor, UseCase), ( safe_name(Actor, SafeActor), safe_name(UseCase, SafeUseCase), format(Stream, '~w -- ~w~n', [SafeActor, SafeUseCase]) )).

% write_includes_puml/1: Writes all include relationships.
write_includes_puml(Stream) :-
    forall(include(BaseUC, IncludedUC), ( safe_name(BaseUC, SafeBaseUC), safe_name(IncludedUC, SafeIncludedUC), format(Stream, '~w ..> ~w : <<include>>~n', [SafeBaseUC, SafeIncludedUC]) )).

% write_extends_puml/1: Writes all extend relationships.
write_extends_puml(Stream) :-
    forall(extend(ExtendingUC, BaseUC), ( safe_name(ExtendingUC, SafeExtendingUC), safe_name(BaseUC, SafeBaseUC), format(Stream, '~w ..> ~w : <<extend>>~n', [SafeExtendingUC, SafeBaseUC]) )).

% write_uc_generalizations_puml/1: Writes all use case generalizations.
write_uc_generalizations_puml(Stream) :-
    forall(generalization(ChildUC, ParentUC), ( safe_name(ChildUC, SafeChildUC), safe_name(ParentUC, SafeParentUC), format(Stream, '~w --|> ~w~n', [SafeChildUC, SafeParentUC]) )).

% write_actor_generalizations_puml/1: Writes all actor generalizations.
write_actor_generalizations_puml(Stream) :-
    forall(actor_generalization(ChildActor, ParentActor), ( safe_name(ChildActor, SafeChildActor), safe_name(ParentActor, SafeParentActor), format(Stream, '~w --|> ~w~n', [SafeChildActor, SafeParentActor]) )).