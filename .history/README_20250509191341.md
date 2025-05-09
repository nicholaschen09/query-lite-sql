# Query Lite SQL

A simple SQL query engine for JSON data with a web interface. This application allows you to run SQL-like queries against flat JSON arrays of objects.

## Features

- Parse and execute SQL queries on JSON data
- Web interface for submitting queries and viewing results
- Query history stored in SQLite database
- Support for SELECT, WHERE, and LIMIT clauses

## Supported SQL Syntax

- `SELECT` (comma separated list of columns or `*`)
- `FROM` (required but not actually used)
- `WHERE` (conditions with `=`, `!=`, `<`, `>`, `AND`, `OR`, parentheses)
- `LIMIT` (integer)

Example queries:
```sql
SELECT state FROM table WHERE pop > 10000000 AND state != 'California';
SELECT * FROM table WHERE pop > 20000000 OR (pop > 10000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
```

## Requirements

- GHC (Glasgow Haskell Compiler)
- Cabal

## Building the Project

To build the project, run:

```bash
cabal build
```

## Running the Application

Run the application with a JSON file as the argument:

```bash
cabal run query-lite-sql-exe -- sample.json
```

Then, open your browser and navigate to:

```
http://localhost:3000/
```

## Project Structure

- `src/` - Source code for the Haskell application
  - `QueryLiteSQL/Parser/SQL.hs` - SQL parser
  - `QueryLiteSQL/Parser/Executor.hs` - Query execution engine
  - `QueryLiteSQL/Database/Schema.hs` - Database schema for query history
  - `QueryLiteSQL/Web/Routes.hs` - Web routes for the API
  - `QueryLiteSQL/Types/Env.hs` - Environment configuration
- `app/` - Main application entry point
- `frontend/` - Web interface
- `sample.json` - Example JSON data file

## Technical Details

- **Backend:** Haskell with Scotty web framework
- **Frontend:** HTML, CSS, JavaScript
- **Database:** SQLite for storing query history
- **Libraries:** 
  - aeson (JSON parsing)
  - attoparsec (SQL parsing)
  - scotty (web framework)
  - sqlite-simple (database access) 