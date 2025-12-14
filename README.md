# Scryer Graph Database

Scryer Graph is a lightweight RDF graph database implemented in [Scryer Prolog](https://scryer.pl/), built primarily as a learning project to explore logic programming, reasoning, and database concepts. It offers a simple REPL interface for storing and querying RDF triples with graph contexts, including basic operations like adding data, pattern matching, and persistence.

**TL;DR**: A hands-on Prolog-based RDF database for adding, querying, and persisting the graph to improve the understanding of Scryer Prolog and logical reasoning.

## Features

- **RDF Triple Storage**: Store triples in the form `(Subject, Predicate, Object, Graph)` using Prolog's dynamic predicates.
- **Basic Querying**: Support for exact matches and placeholders (use "who" to match any value) in queries.
- **Persistence**: Save and load database state to/from files.
- **REPL Interface**: Interactive command-line interface for operations.
- **Extensible**: Built with Prolog for easy addition of reasoning or advanced queries.

## Prerequisites

- [Scryer Prolog](https://scryer.pl/) installed on your system.
- Make (for using the provided Makefile).

## Installation

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd scryer-graph
   ```

2. Build the project:
   ```bash
   make build
   ```

## Usage

### Running the Database

Start the REPL:
```bash
make run
```

In the REPL, enter commands as Prolog terms:
- Add a triple: `add("Alice", "knows", "Bob", "social").`
- Query triples: `query("Alice", "knows", "who", "social").` (finds objects where Alice knows someone in the social graph).
- Save database: `save("backup.txt").`
- Load database: `load("backup.txt").`
- Quit: `quit.`

### Testing

Run basic tests:
```bash
make test
```

This executes `src/test.pl`, which adds sample data and verifies queries.

### Cleaning

Remove generated files:
```bash
make clean
```

## Project Structure

- `src/main.pl`: Main REPL and command handling.
- `src/database.pl`: Core database operations (add, query, save/load).
- `src/test.pl`: Basic test suite.
- `Makefile`: Build, run, test, and clean targets.

## Examples

1. **Adding and Querying**:
   ```
   scryer-graph> add("Alice", "knows", "Bob", "social").
   Added the RDF Graph.
   scryer-graph> query("Alice", "knows", "who", "social").
   Results: [["Alice", "knows", "Bob", "social"]]
   ```

2. **Saving and Loading**:
   ```
   scryer-graph> save("mydata.txt").
   Database saved to mydata.txt.
   scryer-graph> load("mydata.txt").
   Database loaded from mydata.txt.
   ```

## Future Enhancements

- **Reasoning**: Add inference rules for deriving new triples (e.g., transitivity).
- **Advanced Query Language**: Implement a custom DSL or SPARQL subset for complex queries.
- **Performance**: Indexing and optimizations for larger datasets.
- **Web Interface**: REST API or web UI for non-Prolog users.

## Contributing

Contributions are welcome! Please submit issues or pull requests for bugs, features, or improvements. I appreciate feedback from the community. 

## License

This project is open-source under the MIT License. See LICENSE for details.

## Acknowledgments

Built with Scryer Prolog. Inspired by RDF and graph database concepts.