# Use Case Diagram Generator (Prolog)

## Table of Contents

- [Project Description](#project-description)
- [Technologies Used](#technologies-used)
- [How it Works](#how-it-works)
- [Features](#features)
- [How to Run](#how-to-run)

## Project Description

This Prolog project provides an interactive console-based system to create Use Case Diagrams and generate corresponding PlantUML code. Users can define actors, use cases, relationships, and system boundaries dynamically through a menu-driven interface. The program ensures data validation and generates `.puml` files for visualizing the diagrams using PlantUML.

The main goal of this project is to simplify the process of creating UML Use Case Diagrams by providing a user-friendly interface and automating the generation of PlantUML code.

## Technologies Used

- **Prolog (SWI-Prolog)**: For logic programming and data management.
- **PlantUML**: For rendering UML diagrams from the generated `.puml` files.

## How it Works

The program follows these steps:
1. **Interactive Menu**: Users interact with the program through a console-based menu system.
2. **Define Elements**: Users can add actors, use cases, and relationships (e.g., associations, generalizations, includes, and extends).
3. **Validate Input**: The program ensures all relationships are valid and that actors or use cases exist before creating relationships.
4. **Generate PlantUML Code**: Users can generate `.puml` files containing the PlantUML syntax for the defined diagram.
5. **View Diagram Details**: Users can view all defined elements and relationships in a structured format.

## Features

- **Interactive Menu**: A user-friendly menu system for managing elements and relationships.
- **Dynamic Input**: Add and remove actors, use cases, and relationships interactively.
- **System Boundary**: Define a system or package to group use cases.
- **Relationship Handling**: Supports associations, generalizations, includes, and extends.
- **Validation**: Ensures all relationships are valid and prevents duplicate entries.
- **Diagram Export**: Generates `.puml` files for visualization in PlantUML.
- **View Details**: List all elements and relationships or view specific details (e.g., actors, use cases, associations).

## How to Run

1. **Install SWI-Prolog**: Ensure that you have SWI-Prolog installed. If not, you can download it from [here](https://www.swi-prolog.org/Download.html).

2. **Run the Program**:
    Open the terminal and run the Prolog script:
    ```bash
    swipl main.pl
    ```

3. **Interactive Menu**:
    - The program will start with an interactive menu. Follow the prompts to add elements, define relationships, and generate diagrams.
    - Example menu options include:
      - Add actors and use cases.
      - Define relationships (e.g., associations, includes, extends, generalizations).
      - View diagram details.
      - Generate a `.puml` file.

4. **Generate the PlantUML Diagram**:
    - After defining the diagram, choose the option to generate a `.puml` file.
    - Open the `.puml` file in PlantUML to visualize the diagram.

## Example Workflow

1.  **Start the program and initiate the menu**:
    * Open your terminal and navigate to the directory containing `main.pl`.
    * Load the file into SWI-Prolog:
        ```bash
        swipl main.pl
        ```
    * At the Prolog prompt (`?-`), type `start.` to begin the application:
        ```prolog
        start.
        ```
2. Define a system name (e.g., "Library System").
3. Add actors (e.g., "Librarian", "Member").
4. Add use cases (e.g., "Borrow Book", "Return Book").
5. Define relationships.
6. Generate the `.puml` file and visualize it using PlantUML.
