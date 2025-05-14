% -------------------------------
% 1. DEFINE ACTORS AND USE CASES
% -------------------------------

% Actors
actor(passenger).
actor(minor_passenger).
actor(passenger_with_special_needs).
actor(tour_guide).

% Use cases
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

% Alias for actors and use cases
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

% Group all use cases under the 'check_in' package
use_case_in_package(individual_check_in, check_in).
use_case_in_package(counter_check_in, check_in).
use_case_in_package(kiosk_check_in, check_in).
use_case_in_package(group_check_in, check_in).
use_case_in_package(baggage_check_in, check_in).
use_case_in_package(security_screening, check_in).
use_case_in_package(baggage_handling, check_in).

% -------------------------------
% 4. DEFINE RELATIONSHIPS
% -------------------------------

% Generalizations: hierarchical relationships
generalization(minor_passenger, passenger).  % Minor Passenger is a type of Passenger
generalization(passenger_with_special_needs, passenger).  % Special Needs Passenger is a type of Passenger
generalization(tour_guide, passenger).  % Tour Guide is a type of Passenger
generalization(counter_check_in, individual_check_in).  % Counter Check-in is a type of Individual Check-in
generalization(kiosk_check_in, individual_check_in).  % Kiosk Check-in is a type of Individual Check-in

% Include and Extend relationships between use cases
include(group_check_in, individual_check_in).  % Group Check-in always includes Individual Check-in
extend(baggage_check_in, individual_check_in).  % Baggage Check-in extends Individual Check-in
extend(baggage_handling, counter_check_in).  % Baggage Handling extends Counter Check-in
extend(baggage_handling, kiosk_check_in).  % Baggage Handling extends Kiosk Check-in

% Associations: relationships between actors and use cases
association(tour_guide, group_check_in).  % Tour Guide is associated with Group Check-in
association(passenger, individual_check_in).  % Passenger is associated with Individual Check-in
association(passenger, security_screening).  % Passenger is associated with Security Screening
