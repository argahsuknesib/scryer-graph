# Makefile for Scryer Graph Database Project

# Variables
SCRYER = scryer-prolog
MAIN_FILE = src/main.pl
TEST_FILE = src/test.pl

# Default target
all: build

# Build target: Check if the main file loads without errors
build:
	@echo "Building Scryer Graph Database..."
	@$(SCRYER) $(MAIN_FILE) --check || (echo "Build failed"; exit 1)
	@echo "Build successful."

# Run target: Start the REPL
run:
	@echo "Starting Scryer Graph Database REPL..."
	@$(SCRYER) $(MAIN_FILE)

# Test target: Run basic tests (assumes a test.pl file exists)
test:
	@echo "Running tests..."
	@if [ -f $(TEST_FILE) ]; then \
		$(SCRYER) $(TEST_FILE); \
	else \
		echo "No test file found. Creating a basic test..."; \
		echo ":- use_module('src/database')." > $(TEST_FILE); \
		echo "test_add :- database:add_graph('Alice', 'knows', 'Bob', 'social')." >> $(TEST_FILE); \
		echo "test_query :- database:query_graph('Alice', 'knows', 'Bob', 'social')." >> $(TEST_FILE); \
		echo ":- test_add, test_query, halt." >> $(TEST_FILE); \
		$(SCRYER) $(TEST_FILE); \
	fi

# Clean target: Remove generated files
clean:
	@echo "Cleaning up..."
	@rm -f $(TEST_FILE)

.PHONY: all build run test clean
