# Use Case to PlantUML Generator (Prolog)

## Table of Contents

- [Project Description](#project-description)
- [Technologies Used](#technologies-used)
- [How it Works](#how-it-works)
- [Features](#features)
- [How to Run](#how-to-run)
- [Authors](#authors)

## Project Description

This Prolog project converts Use Case Diagram (UseCaseDG) specifications into PlantUML code. The program takes actor and use case definitions, relationships (such as generalization, inclusion, extension, and associations), and generates the corresponding PlantUML code for visualizing the use case diagram.

The main goal of this project is to allow users to input use case specifications in a Prolog-based syntax and automatically generate a corresponding UML diagram using PlantUML, a widely-used tool for creating UML diagrams.

## Technologies Used

- **Prolog (SWI-Prolog)**
- **PlantUML** (for visualizing the UML diagrams)

## How it Works

The program defines a set of Prolog predicates to manage actors, use cases, relationships, and packages. It allows the user to:
- Define actors and use cases.
- Specify relationships between actors and use cases (e.g., generalization, inclusion, extension, and associations).
- Generate the PlantUML syntax to visualize these specifications as a UML use case diagram.

The process follows these steps:
1. **Insert Use Case Specification**: Users define actors, use cases, and relationships using Prolog predicates.
2. **Validate Relations**: The program ensures all relationships are valid by checking that the actors and use cases exist in the database.
3. **Generate PlantUML Code**: Based on the defined specifications, the program generates the PlantUML code that can be used to render the diagram.
4. **Count Use Cases in Packages**: Users can query the number of use cases in a specific package.

## Features

- **Actor and Use Case Definition**: Easily define actors and use cases within the system.
- **Relationship Handling**: Supports generalizations, includes, extends, and associations between actors and use cases.
- **Package Grouping**: Organize use cases into packages to structure the diagram.
- **PlantUML Code Generation**: Automatically generates PlantUML code to represent the use case diagram.
- **Validation**: Ensures all relationships are correctly defined and that actors or use cases are valid.
- **Extensibility**: Can be extended to support additional types of relationships or UML diagram elements.

## How to Run

1. **Install SWI-Prolog**: Ensure that you have SWI-Prolog installed. If not, you can download it from [here](https://www.swi-prolog.org/Download.html).

2. **Clone the Repository**:
    ```
    git clone https://github.com/yourusername/use-case-to-plantuml.git
    cd use-case-to-plantuml
    ```

3. **Run the Program**:
    Open the terminal and run the Prolog script:
    ```
    swipl use_case_to_plantuml.pl
    ```

4. **Generate the PlantUML Diagram**:
    - After running the Prolog code, you can generate the PlantUML code by calling the `generate_plantuml` predicate:
    ```prolog
    ?- generate_plantuml.
    ```

    - The program will output the PlantUML code, which you can copy and paste into a `.puml` file to generate the UML diagram using PlantUML.
