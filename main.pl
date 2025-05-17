:- use_module(library(readutil)). % For reading user input
:- use_module(library(lists)).    % For list utilities

% ==================================================
% ANSI Style Definitions for Console Output
% ==================================================
ansi_style(reset, "\033[0m").
ansi_style(bold, "\033[1m").

ansi_style(fg_black,   "\033[30m").
ansi_style(fg_red,     "\033[31m").
ansi_style(fg_green,   "\033[32m").
ansi_style(fg_yellow,  "\033[33m").
ansi_style(fg_blue,    "\033[34m").
ansi_style(fg_magenta, "\033[35m").
ansi_style(fg_cyan,    "\033[36m").
ansi_style(fg_white,   "\033[37m").

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

% Program entry point.
start :-
    choose_mode.

% Displays the initial menu and prompts for user action.
choose_mode :-
    nl,
    ansi_style(bold, B), ansi_style(fg_cyan, C), ansi_style(reset, R),
    format('~s~s=== Initial Menu ===~s~n', [B, C, R]),
    write('1. Create a new diagram'), nl,
    write('0. Exit Program'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        handle_initial_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), choose_mode
    ).

% Handles the users selection from the initial menu.
handle_initial_choice(1) :-
    clear_data,
    action_set_system_name,
    main_menu.
handle_initial_choice(0) :-
    action_exit_program.
handle_initial_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid option. Please try again.~s~n', [RE, R]),
    choose_mode.

% Prompts for and sets the system/package name.
action_set_system_name :-
    nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sEnter system name for the diagram (this will act as the main package): ~s', [YS, RS]),
    read_line_to_string(user_input, SystemName),
    ( SystemName == "" ->
        ansi_style(fg_red, RE), ansi_style(reset, R),
        format('~sSystem name cannot be empty. Please try again.~s~n', [RE, R]),
        action_set_system_name
    ; (retract(system(_OldName)) -> true ; true),
      assertz(system(SystemName)),
      ansi_style(fg_green, G), ansi_style(reset, R),
      format('~sSystem (main package) set to "~w".~s~n', [G, SystemName, R])
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
    ansi_style(reset, R_Sys), % Local reset for this block
    ( system(SName) ->
        ansi_style(bold, B_Sys), ansi_style(fg_green, G_Sys),
        format('~s~sCurrent System/Package: ~w~s~n', [B_Sys, G_Sys, SName, R_Sys])
    ;   ansi_style(fg_yellow, Y_Sys),
        format('~sNo system/package defined yet. Consider setting one (Option 7).~s~n', [Y_Sys,R_Sys])
    ),
    ansi_style(bold, B), ansi_style(fg_cyan, C), ansi_style(reset, R),
    format('~s~s=== Main Menu ===~s~n', [B, C, R]),
    write('1. Manage Elements (Actors, Use Cases)'), nl,
    write('2. Manage Relationships'), nl,
    write('3. Remove Elements/Relationships'), nl,
    write('4. View Diagram Details'), nl,
    write('5. Generate .puml Diagram File'), nl,
    write('6. Show Use Case Count'), nl,
    write('7. Change/Define System Name'), nl,
    write('8. Return to Initial Menu (clears current diagram)'), nl,
    write('0. Exit Program'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Option) ->
        handle_main_menu_choice(Option)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), main_menu
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
handle_main_menu_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid option number. Please try again.~s~n', [RE, R]), main_menu.

% Clears all data and returns to the initial menu.
action_return_to_initial_menu :-
    ansi_style(fg_yellow, Y), ansi_style(reset, R),
    format('~sClearing current diagram and returning to initial menu...~s~n', [Y,R]),
    clear_data,
    choose_mode.

% Exits the program.
action_exit_program :-
    ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~sGoodbye!~s~n', [B,M,R]).

% ==================================================
% 1. Manage Elements Menu & Actions
% ==================================================
% Menu for adding actors or use cases.
manage_elements_menu :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- Manage Elements ---~s~n', [B, BLU, R]),
    write('1. Add Actor'), nl,
    write('2. Add Use Case'), nl,
    write('0. Back to Main Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_manage_elements_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), manage_elements_menu
    ).

% Handles selection for adding an actor or use case.
process_manage_elements_choice(1) :- action_add_actor, manage_elements_menu.
process_manage_elements_choice(2) :- action_add_use_case, manage_elements_menu.
process_manage_elements_choice(0).
process_manage_elements_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid option. Please try again.~s~n', [RE, R]), manage_elements_menu.

% Adds a new actor to the diagram.
action_add_actor :-
    ansi_style(fg_yellow, Y), ansi_style(reset, R),
    format('~sEnter actor name (or 0 to cancel): ~s', [Y, R]),
    read_line_to_string(user_input, ActorInput),
    ( ActorInput == "0" -> true
    ; ActorInput == "" ->
        ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sActor name cannot be empty.~s~n', [RE, RR])
    ; actor(ActorInput) ->
        ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sActor already exists.~s~n', [RE, RR])
    ; ansi_style(fg_green, G), ansi_style(reset, RG),
      assertz(actor(ActorInput)), format('~sActor added: ~w~s~n', [G, ActorInput, RG])
    ).

% Adds a new use case to the diagram.
action_add_use_case :-
    ansi_style(fg_yellow, Y), ansi_style(reset, R),
    format('~sEnter use case name (or 0 to cancel): ~s', [Y, R]),
    read_line_to_string(user_input, UseCaseInput),
    ( UseCaseInput == "0" -> true
    ; UseCaseInput == "" ->
        ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sUse case name cannot be empty.~s~n', [RE, RR])
    ; use_case(UseCaseInput) ->
        ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sUse case already exists.~s~n', [RE, RR])
    ; ansi_style(fg_green, G), ansi_style(reset, RG),
      assertz(use_case(UseCaseInput)), format('~sUse case added: ~w~s~n', [G, UseCaseInput, RG])
    ).

% ==================================================
% 2. Manage Relationships Menu & Actions
% ==================================================
% Menu for adding associations, includes, extends, and generalizations.
manage_relationships_menu :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- Manage Relationships ---~s~n', [B, BLU, R]),
    write('1. Add Association (Actor -- Use Case)'), nl,
    write('2. Add <<include>> Relation (Use Case ..> Use Case)'), nl,
    write('3. Add <<extend>> Relation (Use Case ..> Use Case)'), nl,
    write('4. Add Generalization (Actor or Use Case)'), nl,
    write('0. Back to Main Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_manage_relationships_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), manage_relationships_menu
    ).

% Handles selection for relationship management.
process_manage_relationships_choice(1) :- action_add_association, manage_relationships_menu.
process_manage_relationships_choice(2) :- action_add_include, manage_relationships_menu.
process_manage_relationships_choice(3) :- action_add_extend, manage_relationships_menu.
process_manage_relationships_choice(4) :- action_manage_generalizations, manage_relationships_menu.
process_manage_relationships_choice(0).
process_manage_relationships_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid option. Please try again.~s~n', [RE,R]), manage_relationships_menu.

% Adds an association between an actor and a use case.
action_add_association :-
    ( list_actors_or_prompt_add ->
        ansi_style(fg_yellow, Y), ansi_style(reset, R),
        format('~sChoose actor by number for association (or 0 to cancel): ~s', [Y, R]),
        read_line_to_string(user_input, AI),
        ( AI == "0" -> true
        ; atom_number(AI, AIndex), get_nth_actor(AIndex, Actor) ->
            ( list_use_cases_or_prompt_add ->
                ansi_style(fg_yellow, Y2), ansi_style(reset, R2),
                format('~sChoose use case by number for association (or 0 to cancel): ~s', [Y2, R2]),
                read_line_to_string(user_input, UI),
                ( UI == "0" -> true
                ; atom_number(UI, UIndex), get_nth_use_case(UIndex, UseCase) ->
                    ( association(Actor, UseCase) ->
                        ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sThis association already exists.~s~n', [RE, RR])
                    ; ansi_style(fg_green, G), ansi_style(reset, RG), assertz(association(Actor, UseCase)),
                      format('~sAssociation added: Actor "~w" -- Use Case "~w"~s~n', [G, Actor, UseCase, RG])
                    )
                ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid use case selection.~s~n', [RE, RR])
                )
            ; true
            )
        ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid actor selection.~s~n', [RE, RR])
        )
    ; true
    ).

% Adds an include relationship between use cases.
action_add_include :-
    ( list_use_cases_or_prompt_add(2) ->
        ansi_style(fg_yellow, Y), ansi_style(reset, R),
        format('~sChoose base use case (FROM which includes) (number) (or 0 to cancel): ~s', [Y, R]),
        read_line_to_string(user_input, BaseUI),
        ( BaseUI == "0" -> true
        ; atom_number(BaseUI, BaseIndex), get_nth_use_case(BaseIndex, BaseUseCase) ->
            ansi_style(fg_yellow, Y2), ansi_style(reset, R2),
            format('~sChoose use case to be included (TO) (number) (or 0 to cancel): ~s', [Y2, R2]),
            read_line_to_string(user_input, InclUI),
            ( InclUI == "0" -> true
            ; atom_number(InclUI, InclIndex), get_nth_use_case(InclIndex, IncludedUseCase) ->
                ( BaseUseCase == IncludedUseCase ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sA use case cannot include itself.~s~n', [RE, RR])
                ; include(BaseUseCase, IncludedUseCase) ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sThis <<include>> relation already exists.~s~n', [RE, RR])
                ; ansi_style(fg_green, G), ansi_style(reset, RG), assertz(include(BaseUseCase, IncludedUseCase)),
                  format('~sRelation added: "~w" ..> "~w" : <<include>>~s~n', [G, BaseUseCase, IncludedUseCase, RG])
                )
            ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid "TO" use case selection.~s~n', [RE, RR])
            )
        ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid "FROM" use case selection.~s~n', [RE, RR])
        )
    ; true
    ).

% Adds an extend relationship between use cases.
action_add_extend :-
    ( list_use_cases_or_prompt_add(2) ->
        ansi_style(fg_yellow, Y), ansi_style(reset, R),
        format('~sChoose extending use case (FROM which extends) (number) (or 0 to cancel): ~s', [Y, R]),
        read_line_to_string(user_input, ExtUI),
        ( ExtUI == "0" -> true
        ; atom_number(ExtUI, ExtIndex), get_nth_use_case(ExtIndex, ExtendingUseCase) ->
            ansi_style(fg_yellow, Y2), ansi_style(reset, R2),
            format('~sChoose base use case to be extended (TO) (number) (or 0 to cancel): ~s', [Y2, R2]),
            read_line_to_string(user_input, BaseUI),
            ( BaseUI == "0" -> true
            ; atom_number(BaseUI, BaseIndex), get_nth_use_case(BaseIndex, BaseUseCase) ->
                ( ExtendingUseCase == BaseUseCase ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sA use case cannot extend itself.~s~n', [RE, RR])
                ; extend(ExtendingUseCase, BaseUseCase) ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sThis <<extend>> relation already exists.~s~n', [RE, RR])
                ; ansi_style(fg_green, G), ansi_style(reset, RG), assertz(extend(ExtendingUseCase, BaseUseCase)),
                  format('~sRelation added: "~w" ..> "~w" : <<extend>>~s~n', [G, ExtendingUseCase, BaseUseCase, RG])
                )
            ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid "TO" use case selection.~s~n', [RE, RR])
            )
        ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid "FROM" use case selection.~s~n', [RE, RR])
        )
    ; true
    ).

% Menu for adding generalizations between actors or use cases.
action_manage_generalizations :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- Add Generalization ---~s~n', [B, BLU, R]),
    write('1. Actor Generalization'), nl,
    write('2. Use Case Generalization'), nl,
    write('0. Back to Relationships Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose type of generalization: ~s', [YS, RS]),
    read_line_to_string(user_input, GenTypeInput),
    ( atom_number(GenTypeInput, GenTypeChoice) ->
        process_generalization_type_choice(GenTypeChoice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), action_manage_generalizations
    ).

% Handles the type of generalization to add.
process_generalization_type_choice(1) :- add_actor_generalization_interaction, action_manage_generalizations.
process_generalization_type_choice(2) :- add_use_case_generalization_interaction, action_manage_generalizations.
process_generalization_type_choice(0).
process_generalization_type_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid generalization type choice.~s~n', [RE, R]), action_manage_generalizations.

% Adds a generalization between two actors.
add_actor_generalization_interaction :-
    ( list_actors_or_prompt_add(2) ->
        ansi_style(fg_yellow, Y), ansi_style(reset, R),
        format('~sChoose child actor (specific type) (number) (or 0 to cancel): ~s', [Y, R]),
        read_line_to_string(user_input, CI),
        ( CI == "0" -> !
        ; atom_number(CI, CIndex), get_nth_actor(CIndex, ChildActor) ->
            ansi_style(fg_yellow, Y2), ansi_style(reset, R2),
            format('~sChoose parent actor (general type) (number) (or 0 to cancel): ~s', [Y2, R2]),
            read_line_to_string(user_input, PI),
            ( PI == "0" -> !
            ; atom_number(PI, PIndex), get_nth_actor(PIndex, ParentActor) ->
                ( ChildActor == ParentActor ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sChild and parent actor cannot be the same.~s~n', [RE, RR])
                ; actor_generalization(ChildActor, ParentActor) ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sThis actor generalization already exists.~s~n', [RE, RR])
                ; ansi_style(fg_green, G), ansi_style(reset, RG), assertz(actor_generalization(ChildActor, ParentActor)),
                  format('~sActor generalization added: "~w" --|> "~w"~s~n', [G, ChildActor, ParentActor, RG])
                )
            ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid parent actor selection.~s~n', [RE, RR])
            )
        ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid child actor selection.~s~n', [RE, RR])
        )
    ; !
    ).

% Adds a generalization between two use cases.
add_use_case_generalization_interaction :-
    ( list_use_cases_or_prompt_add(2) ->
        ansi_style(fg_yellow, Y), ansi_style(reset, R),
        format('~sChoose child use case (specific type) (number) (or 0 to cancel): ~s', [Y, R]),
        read_line_to_string(user_input, CI),
        ( CI == "0" -> !
        ; atom_number(CI, CIndex), get_nth_use_case(CIndex, ChildUseCase) ->
            ansi_style(fg_yellow, Y2), ansi_style(reset, R2),
            format('~sChoose parent use case (general type) (number) (or 0 to cancel): ~s', [Y2, R2]),
            read_line_to_string(user_input, PI),
            ( PI == "0" -> !
            ; atom_number(PI, PIndex), get_nth_use_case(PIndex, ParentUseCase) ->
                ( ChildUseCase == ParentUseCase ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sChild and parent use case cannot be the same.~s~n', [RE, RR])
                ; generalization(ChildUseCase, ParentUseCase) ->
                    ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sThis use case generalization already exists.~s~n', [RE, RR])
                ; ansi_style(fg_green, G), ansi_style(reset, RG), assertz(generalization(ChildUseCase, ParentUseCase)),
                  format('~sUse case generalization added: "~w" --|> "~w"~s~n', [G, ChildUseCase, ParentUseCase, RG])
                )
            ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid parent use case selection.~s~n', [RE, RR])
            )
        ; ansi_style(fg_red, RE), ansi_style(reset, RR), format('~sInvalid child use case selection.~s~n', [RE, RR])
        )
    ; !
    ).

% ==================================================
% Section for Viewing Diagram Details (Main Menu Option 4)
% ==================================================
% Provides options to view all or specific details of the diagram.
view_diagram_details_top_menu :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- View Diagram Details ---~s~n', [B, BLU, R]),
    write('1. List Everything'), nl,
    write('2. List Specific Details'), nl,
    write('0. Back to Main Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_view_details_top_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), view_diagram_details_top_menu
    ).

% Handles user selection for viewing diagram details.
process_view_details_top_choice(1) :- list_everything_detail, view_diagram_details_top_menu.
process_view_details_top_choice(2) :- view_specific_details_menu, view_diagram_details_top_menu.
process_view_details_top_choice(0).
process_view_details_top_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid choice.~s~n', [RE, R]), view_diagram_details_top_menu.

% Provides options to list specific diagram components.
view_specific_details_menu :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- List Specific Details ---~s~n', [B, BLU, R]),
    write('1. List All Actors'), nl,
    write('2. List All Use Cases'), nl,
    write('3. List Associations (Actor -- Use Case)'), nl,
    write('4. List <<include>> Relations'), nl,
    write('5. List <<extend>> Relations'), nl,
    write('6. List Actor Generalizations (Actor --|> Actor)'), nl,
    write('7. List Use Case Generalizations (Use Case --|> Use Case)'), nl,
    write('0. Back to View Diagram Details Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_specific_details_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), view_specific_details_menu
    ).

% Handles user selection for listing specific details.
process_specific_details_choice(1) :- list_all_actors_detail, view_specific_details_menu.
process_specific_details_choice(2) :- list_all_use_cases_detail, view_specific_details_menu.
process_specific_details_choice(3) :- list_all_associations_detail, view_specific_details_menu.
process_specific_details_choice(4) :- list_all_includes_detail, view_specific_details_menu.
process_specific_details_choice(5) :- list_all_extends_detail, view_specific_details_menu.
process_specific_details_choice(6) :- list_all_actor_generalizations_detail, view_specific_details_menu.
process_specific_details_choice(7) :- list_all_uc_generalizations_detail, view_specific_details_menu.
process_specific_details_choice(0).
process_specific_details_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid choice.~s~n', [RE, R]), view_specific_details_menu.

% Prints all defined actors.
list_all_actors_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Actors ---~s~n', [B, M, R]),
    findall(Actor, actor(Actor), Actors),
    ( Actors == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo actors defined.~s~n', [Y,RY])
    ; print_list_indexed(Actors, 1)
    ), nl.

% Prints all defined use cases.
list_all_use_cases_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Use Cases ---~s~n', [B, M, R]),
    findall(UseCase, use_case(UseCase), UseCases),
    ( UseCases == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo use cases defined.~s~n', [Y,RY])
    ; print_list_indexed(UseCases, 1)
    ), nl.

% Shows the count of use cases in the defined system.
count_use_cases_in_system_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Count of Use Cases in System/Package ---~s~n', [B, M, R]),
    ( system(SystemName) ->
        findall(UseCase, use_case(UseCase), UseCases),
        length(UseCases, Count),
        ansi_style(reset, RS),
        format('~sThe system/package "~w" contains ~d use case(s).~s~n', [RS, SystemName, Count, RS])
    ; ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo system/package defined. Cannot count use cases.~s~n', [Y,RY])
    ), nl.

% list_all_associations_detail/0: Lists all actor-use case associations.
list_all_associations_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Associations (Actor -- Use Case) ---~s~n', [B, M, R]),
    findall(association(Actor, UC), association(Actor, UC), Associations),
    ( Associations == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo associations defined.~s~n', [Y,RY])
    ; print_relationship_list_indexed(Associations, 1, '--')
    ), nl.

% list_all_includes_detail/0: Lists all include relationships.
list_all_includes_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- <<include>> Relations (Base UC ..> Included UC) ---~s~n', [B, M, R]),
    findall(include(Base, Incl), include(Base, Incl), Includes),
    ( Includes == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo <<include>> relations defined.~s~n', [Y,RY])
    ; print_relationship_list_indexed(Includes, 1, '..> <<include>>')
    ), nl.

% list_all_extends_detail/0: Lists all extend relationships.
list_all_extends_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- <<extend>> Relations (Extending UC ..> Base UC) ---~s~n', [B, M, R]),
    findall(extend(Extending, Base), extend(Extending, Base), Extends),
    ( Extends == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo <<extend>> relations defined.~s~n', [Y,RY])
    ; print_relationship_list_indexed(Extends, 1, '..> <<extend>>')
    ), nl.

% list_all_actor_generalizations_detail/0: Lists all actor generalizations.
list_all_actor_generalizations_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Actor Generalizations (Child Actor --|> Parent Actor) ---~s~n', [B, M, R]),
    findall(actor_generalization(Child, Parent), actor_generalization(Child, Parent), ActorGens),
    ( ActorGens == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo actor generalizations defined.~s~n', [Y,RY])
    ; print_relationship_list_indexed(ActorGens, 1, '--|>')
    ), nl.

% list_all_uc_generalizations_detail/0: Lists all use case generalizations.
list_all_uc_generalizations_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Use Case Generalizations (Child UC --|> Parent UC) ---~s~n', [B, M, R]),
    findall(generalization(Child, Parent), generalization(Child, Parent), UCGens),
    ( UCGens == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo use case generalizations defined.~s~n', [Y,RY])
    ; print_relationship_list_indexed(UCGens, 1, '--|>')
    ), nl.

% list_everything_detail/0: Lists all elements and relationships in the diagram.
list_everything_detail :-
    nl, ansi_style(bold, B), ansi_style(fg_cyan, C), ansi_style(reset, R),
    format("~s~s=============================~s~n", [B,C,R]),
    format("~s~s=== Full Diagram Summary ===~s~n", [B,C,R]),
    format("~s~s=============================~s~n", [B,C,R]),
    ansi_style(reset, R_Sys), 
    (system(SName) ->
        ansi_style(bold, B_Sys), ansi_style(fg_green, G_Sys),
        format('~s~sSystem/Package: ~w~s~n', [B_Sys, G_Sys, SName, R_Sys])
    ;   ansi_style(fg_yellow, Y_Sys),
        format('~sSystem/Package: Not defined~s~n', [Y_Sys,R_Sys])
    ), nl,
    list_all_actors_detail,
    list_all_use_cases_detail,
    count_use_cases_in_system_detail,
    list_all_associations_detail,
    list_all_includes_detail,
    list_all_extends_detail,
    list_all_actor_generalizations_detail,
    list_all_uc_generalizations_detail,
    ansi_style(bold, BOLD_SUM), ansi_style(fg_cyan, CYAN_SUM), ansi_style(reset, R_SUM),
    format("~s~s===== End of Summary =====~s~n", [BOLD_SUM, CYAN_SUM, R_SUM]).


% ==================================================
% Section for Removing Elements and Relationships.
% ==================================================
manage_remove_menu :-
    nl,
    ansi_style(bold, B), ansi_style(fg_blue, BLU), ansi_style(reset, R),
    format('~s~s--- Remove Elements/Relationships ---~s~n', [B, BLU, R]),
    write('1. Remove Actor (and related connections)'), nl,
    write('2. Remove Use Case (and related connections)'), nl,
    write('3. Remove Association'), nl,
    write('4. Remove <<include>> Relation'), nl,
    write('5. Remove <<extend>> Relation'), nl,
    write('6. Remove Actor Generalization'), nl,
    write('7. Remove Use Case Generalization'), nl,
    write('0. Back to Main Menu'), nl,
    ansi_style(fg_yellow, YS), ansi_style(reset, RS),
    format('~sChoose an option: ~s', [YS, RS]),
    read_line_to_string(user_input, Input),
    ( atom_number(Input, Choice) ->
        process_remove_menu_choice(Choice)
    ; ansi_style(fg_red, RE), ansi_style(reset, RR),
      format('~sInvalid input. Please enter a number.~s~n', [RE, RR]), manage_remove_menu
    ).

process_remove_menu_choice(1) :- action_remove_actor, manage_remove_menu.
process_remove_menu_choice(2) :- action_remove_use_case, manage_remove_menu.
process_remove_menu_choice(3) :- action_remove_association, manage_remove_menu.
process_remove_menu_choice(4) :- action_remove_include, manage_remove_menu.
process_remove_menu_choice(5) :- action_remove_extend, manage_remove_menu.
process_remove_menu_choice(6) :- action_remove_actor_generalization, manage_remove_menu.
process_remove_menu_choice(7) :- action_remove_uc_generalization, manage_remove_menu.
process_remove_menu_choice(0).
process_remove_menu_choice(_) :-
    ansi_style(fg_red, RE), ansi_style(reset, R),
    format('~sInvalid option. Please try again.~s~n', [RE,R]), manage_remove_menu.

action_remove_actor :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Remove Actor ---~s~n', [B, M, R]),
    findall(Actor, actor(Actor), Actors),
    ( Actors == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo actors to remove.~s~n', [Y,RY])
    ; ansi_style(reset, RS), write(RS), write('Actors:'), nl,
      print_list_indexed(Actors, 1),
      ansi_style(fg_yellow, YP), ansi_style(reset, RP),
      format('~sEnter number of actor to remove (or 0 to cancel): ~s', [YP, RP]),
      read_line_to_string(user_input, IndexStr),
      ( IndexStr == "0" -> ansi_style(fg_yellow, YS), ansi_style(reset, RS_Cancel), format('~sCancelled.~s~n', [YS,RS_Cancel]) % Changed RS to RS_Cancel
      ; atom_number(IndexStr, Index), get_nth_item(actor, Index, ActorToRemove) ->
          retract(actor(ActorToRemove)),
          retractall(association(ActorToRemove, _)),
          retractall(association(_, ActorToRemove)),
          retractall(actor_generalization(ActorToRemove, _)),
          retractall(actor_generalization(_, ActorToRemove)),
          ansi_style(fg_green, G), ansi_style(reset, RG),
          format('~sActor "~w" and all its connections removed.~s~n', [G, ActorToRemove, RG])
      ; ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sInvalid actor selection or actor not found.~s~n', [RE,RR])
      )
    ).

action_remove_use_case :-
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Remove Use Case ---~s~n', [B, M, R]),
    findall(UC, use_case(UC), UseCases),
    ( UseCases == [] -> ansi_style(fg_yellow, Y), ansi_style(reset, RY), format('~sNo use cases to remove.~s~n', [Y,RY])
    ; ansi_style(reset, RS), write(RS), write('Use Cases:'), nl,
      print_list_indexed(UseCases, 1),
      ansi_style(fg_yellow, YP), ansi_style(reset, RP),
      format('~sEnter number of use case to remove (or 0 to cancel): ~s', [YP, RP]),
      read_line_to_string(user_input, IndexStr),
      ( IndexStr == "0" -> ansi_style(fg_yellow, YS), ansi_style(reset, RS_Cancel), format('~sCancelled.~s~n', [YS,RS_Cancel])
      ; atom_number(IndexStr, Index), get_nth_item(use_case, Index, UCToRemove) ->
          retract(use_case(UCToRemove)),
          retractall(association(_, UCToRemove)),
          retractall(include(UCToRemove, _)),
          retractall(include(_, UCToRemove)),
          retractall(extend(UCToRemove, _)),
          retractall(extend(_, UCToRemove)),
          retractall(generalization(UCToRemove, _)),
          retractall(generalization(_, UCToRemove)),
          ansi_style(fg_green, G), ansi_style(reset, RG),
          format('~sUse Case "~w" and all its connections removed.~s~n', [G, UCToRemove, RG])
      ; ansi_style(fg_red, RE), ansi_style(reset, RR),
        format('~sInvalid use case selection or use case not found.~s~n', [RE,RR])
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
    nl, ansi_style(bold, B), ansi_style(fg_magenta, M), ansi_style(reset, R),
    format('~s~s--- Remove ~w ---~s~n', [B, M, DisplayName, R]),
    Goal =.. [Functor, _Arg1, _Arg2],
    findall(Goal, Goal, Relationships),
    ( Relationships == [] ->
        ansi_style(fg_yellow, Y), ansi_style(reset, RY),
        format('~sNo ~ws to remove.~s~n', [Y, DisplayName, RY])
    ;   ansi_style(reset, RS),
        format('~s~ws:~s~n', [RS, DisplayName, RS]),
        print_relationship_list_indexed(Relationships, 1, Separator),
        ansi_style(fg_yellow, YP), ansi_style(reset, RP),
        format('~sEnter number of relationship to remove (or 0 to cancel): ~s', [YP, RP]),
        read_line_to_string(user_input, IndexStr),
        ( IndexStr == "0" -> ansi_style(fg_yellow, YS), ansi_style(reset, RS_Cancel), format('~sCancelled.~s~n', [YS,RS_Cancel])
        ; atom_number(IndexStr, Index), nth1(Index, Relationships, RelToRemove) ->
            retract(RelToRemove),
            RelToRemove =.. [_, RelArg1, RelArg2],
            ansi_style(fg_green, G), ansi_style(reset, RG),
            format('~s~w: "~w" ~w "~w" removed.~s~n', [G, DisplayName, RelArg1, Separator, RelArg2, RG])
        ; ansi_style(fg_red, RE), ansi_style(reset, RR),
          format('~sInvalid selection or relationship not found.~s~n', [RE,RR])
        )
    ).

% action_generate_puml_file/0: Prompts for filename and generates the PlantUML file.
action_generate_puml_file :-
    ( \+ system(_) ->
        ansi_style(fg_red, RE), ansi_style(reset, R),
        format('~sNo system/package defined. Please define a system name first (Main Menu option 7).~s~n', [RE, R])
    ; system(SystemName),
      ansi_style(reset, RS),
      format('~sThe default filename will be "~w.puml".~s~n', [RS, SystemName, RS]),
      ansi_style(fg_yellow, YP), ansi_style(reset, R_Prompt), % Changed R to R_Prompt
      format('~sEnter a custom filename (without .puml extension) or press Enter to use the default (or 0 to cancel): ~s', [YP, R_Prompt]),
      read_line_to_string(user_input, CustomNameInput),
      ( CustomNameInput == "0" -> true
      ; CustomNameInput == "" -> generate_puml_file(SystemName)
      ; generate_puml_file(CustomNameInput)
      )
    ).

% -------------------------------
% Helpers
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
        ansi_style(fg_red, RE), ansi_style(reset, R),
        format('~sNot enough ~w defined (need at least ~d, found ~d). Please add more ~w first from "Manage Elements" menu.~s~n', [RE, PluralName, MinCount, Count, PluralName, R]),
        fail
    ; ansi_style(reset, RS),
      format('~s~w:~s~n', [RS, PluralName, RS]),
      print_list_indexed(ItemsList, 1),
      true
    ).

% print_list_indexed/2: Prints a list with indices.
print_list_indexed([], _).
print_list_indexed([H|T], N) :-
    ansi_style(reset, R),
    format('~s~d. ~w~s~n', [R, N, H, R]),
    N1 is N + 1,
    print_list_indexed(T, N1).

% Helper to print relationship lists
print_relationship_list_indexed([], _, _).
print_relationship_list_indexed([Rel|T], N, Separator) :-
    Rel =.. [_Functor, Arg1Rel, Arg2Rel],
    ansi_style(reset, R),
    format('~s~d. ~w ~w ~w~s~n', [R, N, Arg1Rel, Separator, Arg2Rel, R]),
    N1 is N + 1,
    print_relationship_list_indexed(T, N1, Separator).


% get_nth_actor/2, get_nth_use_case/2: Retrieves the N-th actor/use case.
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
        ansi_style(fg_red, RE), ansi_style(reset, R),
        format('~sError: System name not defined. Cannot generate file.~s~n', [RE,R])
    ; format(atom(FileName), '~w.puml', [BaseFileName]),
      open(FileName, write, Stream),
      write_puml_content(Stream),
      close(Stream),
      (system(SysName) -> DiagramName = SysName ; DiagramName = BaseFileName),
      ansi_style(fg_green, G), ansi_style(reset, R_Succ),
      format('~sDiagram "~w" saved to file "~w"~s~n', [G, DiagramName, FileName, R_Succ])
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