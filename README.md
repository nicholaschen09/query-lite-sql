# Query-Lite-SQL

A lightweight Haskell application that enables SQL querying on flat JSON data.

## Features

- SQL query support (SELECT statements with WHERE and LIMIT)
- Web interface for submitting queries
- History of past queries
- SQLite database for persistence

## Building and Running

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal
- Node.js and npm (for the frontend)

### Building

```bash
# Build the Haskell backend
cabal build

# Build the SvelteKit frontend
cd static
npm install
npm run build
```

### Running

```bash
# Run with a JSON file as input
cabal run query-lite-sql-exe -- /path/to/data.json
```

The application will be available at http://localhost:3000

## Supported SQL Syntax

```sql
SELECT column1, column2, ... FROM table WHERE condition LIMIT n
SELECT * FROM table WHERE condition
```

Supported conditions:
- Comparison operators: =, !=, <, >
- Logical operators: AND, OR
- Parentheses for grouping

## Example Queries

```sql
SELECT state FROM table WHERE pop > 1000000 AND state != 'California'
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest')
SELECT * FROM table WHERE pop_male > pop_female
``` 