# Scryer Graph Database

A lightweight RDF graph database implemented in Scryer Prolog, designed as a learning project to explore logic programming, reasoning, and knowledge representation. The system supports storing RDF triples with graph contexts, pattern-based querying, automated reasoning with both hardcoded and custom rules, and database persistence.

## Overview

Scryer Graph provides a modular architecture for working with RDF data in Prolog. It includes a core database engine, a reasoning system with support for transitive properties and custom rule loading, and an interactive REPL interface.

## Features

- RDF Triple Storage: Store triples in the form (Subject, Predicate, Object, Graph) using dynamic Prolog predicates
- Pattern Matching: Query with exact matches or use "who" as a wildcard to match any value
- Hardcoded Reasoning: Built-in inference for transitive properties, symmetric properties, inverse properties, subPropertyOf, and subClassOf relationships
- Custom Rule Loading: Load and apply user-defined inference rules from external files via a meta-interpreter
- Persistence: Save and load database state to/from files
- Interactive REPL: Command-line interface for all database operations
- Modular Design: Separate modules for database, reasoning, rule loading, and parsing

## Prerequisites

- Scryer Prolog installed on your system
- Make (optional, for using the Makefile)

## Installation

Clone the repository:
```
git clone <repository-url>
cd scryer-graph
```

Build the project (optional check):
```
make build
```

## Usage

### Running the Database

Start the REPL:
```
make run
```

Or directly:
```
scryer-prolog src/main.pl
```

### REPL Commands

All commands should be entered as Prolog terms ending with a period.

Add a triple:
```
add("Alice", "knows", "Bob", "social").
```

Query triples (use "who" as wildcard):
```
query("Alice", "knows", "who", "social").
```

Query with reasoning (uses hardcoded inference rules):
```
reason("Alice", "knows", "who", "social").
```

Save database to file:
```
save("backup.txt").
```

Load database from file:
```
load("backup.txt").
```

Exit the REPL:
```
quit.
```

### Testing

The test suite verifies both basic database operations and reasoning capabilities:

```
scryer-prolog -g "consult('src/test.pl'), run_tests, halt"
```

The tests validate:
- Adding triples to the database
- Direct fact queries
- Hardcoded transitive reasoning (Alice knows Bob, Bob knows Charlie, therefore Alice knows Charlie)
- Custom rule loading and meta-interpreter reasoning (ancestor and sibling rules)

## Project Structure

```
scryer-graph/
├── src/
│   ├── main.pl         - REPL interface and command handling
│   ├── database.pl     - Core database operations (add, query, save, load)
│   ├── reasoner.pl     - Inference engine with hardcoded rules and meta-interpreter
│   ├── rule_loader.pl  - Loads custom rules from external files
│   ├── parser.pl       - Argument parsing utilities
│   └── test.pl         - Test suite
├── test_rules.pl       - Example custom rules (ancestor, sibling)
├── Makefile            - Build automation
├── LICENSE.md          - MIT License
└── README.md           - This file
```

## Module Details

database.pl: Provides the core RDF storage using dynamic predicates with add_graph/4, query_graph/4, save_database/1, and load_database/1.

reasoner.pl: Contains two reasoning modes:
- solve/4: Hardcoded inference for common RDF/OWL patterns
- solve_with_rules/5: Meta-interpreter that applies custom rules loaded from files

rule_loader.pl: Parses Prolog rule files and converts them to internal rule representation for the meta-interpreter.

parser.pl: Helper utilities for parsing command arguments in the REPL.

main.pl: Interactive loop that accepts commands and dispatches to appropriate modules.

## Example Workflows

### Basic Operations
```
scryer-graph> add("Alice", "knows", "Bob", "social").
Added the RDF Graph.
scryer-graph> add("Bob", "knows", "Charlie", "social").
Added the RDF Graph.
scryer-graph> query("Alice", "knows", "who", "social").
Results: [["Alice", "knows", "Bob", "social"]]
```

### Transitive Reasoning
```
scryer-graph> add("Alice", "knows", "Bob", "social").
scryer-graph> add("Bob", "knows", "Charlie", "social").
scryer-graph> add("knows", "type", "TransitiveProperty", "social").
scryer-graph> reason("Alice", "knows", "who", "social").
Results: [["Alice", "knows", "Bob", "social"], ["Alice", "knows", "Charlie", "social"]]
```

### Custom Rules
```
scryer-graph> add("Homer", "parent", "Bart", "simpsons").
scryer-graph> add("Abe", "parent", "Homer", "simpsons").
scryer-graph> reason_with_rules("Abe", "ancestor", "who", "simpsons", "test_rules.pl").
Results: [["Abe", "ancestor", "Homer", "simpsons"], ["Abe", "ancestor", "Bart", "simpsons"]]
```

### Persistence
```
scryer-graph> save("mydata.txt").
Database saved to mydata.txt.
scryer-graph> quit.

# Later session
scryer-graph> load("mydata.txt").
Database loaded from mydata.txt.
scryer-graph> query("who", "who", "who", "who").
# All previously saved triples are restored
```

## Reasoning Capabilities

Hardcoded inference rules include:
- Transitive Properties: If P is transitive and (S, P, I) and (I, P, O) exist, infer (S, P, O)
- Symmetric Properties: If P is symmetric and (S, P, O) exists, infer (O, P, S)
- Inverse Properties: If P1 inverseOf P2 and (S, P1, O) exists, infer (O, P2, S)
- SubPropertyOf: If P1 subPropertyOf P2 and (S, P1, O) exists, infer (S, P2, O)
- SubClassOf: If C1 subClassOf C2 and (S, type, C1) exists, infer (S, type, C2)

Custom rules can be defined in Prolog syntax and loaded at runtime. See test_rules.pl for examples.

## Future Enhancements

- SPARQL query support for more expressive querying
- Additional OWL reasoning patterns (inverse functional properties, cardinality constraints)
- Indexing and optimization for larger datasets
- Graph visualization capabilities
- RESTful API for remote access
- WASM compilation for browser-based usage

## License

This project is open-source under the MIT License. See LICENSE.md for details.

## Learning Resources

- Scryer Prolog documentation: https://scryer.pl/
- RDF Primer: https://www.w3.org/TR/rdf11-primer/
- OWL Web Ontology Language: https://www.w3.org/OWL/
- Logic Programming and Prolog: Learn Prolog Now! (http://www.learnprolognow.org/)