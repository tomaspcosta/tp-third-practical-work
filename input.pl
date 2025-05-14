% -------------------------------
% 1. DEFINE ACTORS AND USE CASES
% -------------------------------

actor(passenger).
actor(minor_passenger).
actor(passenger_with_special_needs).
actor(tour_guide).

use_case(individual_check_in).
use_case(counter_check_in).
use_case(kiosk_check_in).
use_case(group_check_in).
use_case(baggage_check_in).
use_case(security_screening).
use_case(baggage_handling).

% -------------------------------
% 2. DEFINE ALIASES
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
% 4. DEFINE RELATIONSHIPS
% -------------------------------

% Generalizations define hierarchical relationships between actors or use cases
generalization(minor_passenger, passenger).
generalization(passenger_with_special_needs, passenger).
generalization(tour_guide, passenger).
generalization(counter_check_in, individual_check_in).
generalization(kiosk_check_in, individual_check_in).

% Include and Extend relationships between use cases
include(group_check_in, individual_check_in).
extend(baggage_check_in, individual_check_in).
extend(baggage_handling, counter_check_in).
extend(baggage_handling, kiosk_check_in).

% Associations define relationships between actors and use cases
association(tour_guide, group_check_in).
association(passenger, individual_check_in).
association(passenger, security_screening).

% -------------------------------
% 5. SCENERY DEFINITION
% -------------------------------

scenery('<<Business>>\\nAirport').

% -------------------------------
% 6. USE CASE GROUPING UNDER SPECIFIC SCENERY
% -------------------------------

use_case_in_scenery(individual_check_in, '<<Business>>\\nAirport').
use_case_in_scenery(counter_check_in, '<<Business>>\\nAirport').
use_case_in_scenery(kiosk_check_in, '<<Business>>\\nAirport').
use_case_in_scenery(group_check_in, '<<Business>>\\nAirport').
use_case_in_scenery(baggage_check_in, '<<Business>>\\nAirport').
use_case_in_scenery(security_screening, '<<Business>>\\nAirport').
use_case_in_scenery(baggage_handling, '<<Business>>\\nAirport').
