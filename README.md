# Query Lite SQL

A simple SQL query interface for JSON data with a web interface. This application allows you to query JSON data using SQL-like syntax and provides a web interface for executing queries and viewing query history.

## Features

- SQL-like query interface for JSON data
- Web interface for executing queries
- Query history tracking
- Support for basic SQL operations (SELECT, WHERE, LIMIT)
- Error handling and validation

## Prerequisites

- GHC 8.10 or later
- Cabal 3.0 or later
- SQLite 3

## Building

1. Clone the repository:
```bash
git clone https://github.com/yourusername/query-lite-sql.git
cd query-lite-sql
```

2. Build the project:
```bash
cabal build
```

## Running

1. Prepare your JSON data file. The file should contain an array of objects with consistent keys and only string/number values.

2. Run the application:
```bash
cabal run query-lite-sql-exe -- path/to/your/data.json
```

3. Open your web browser and navigate to `http://localhost:3000`

## Query Syntax

The application supports a subset of SQL syntax:

```sql
SELECT column1, column2 FROM table WHERE condition LIMIT number;
```

Supported operations:
- SELECT: comma-separated list of columns or *
- FROM: table name
- WHERE: conditions using =, !=, <, >, AND, OR, and parentheses
- LIMIT: number of results to return

Example queries:
```sql
SELECT state FROM table WHERE pop > 1000000 AND state != 'California';
SELECT * FROM table WHERE pop > 1000000000 OR (pop > 1000000 AND region = 'Midwest');
SELECT * FROM table WHERE pop_male > pop_female;
```

## Limitations

- No support for GROUP BY, JOIN, or subqueries
- Only supports string and number values in JSON
- No support for NULL values
- No support for nested objects or arrays
- Column names cannot be SQL reserved keywords

## Error Handling

The application will provide error messages for:
- Invalid SQL syntax
- Unknown columns
- Type mismatches
- Empty result sets
- Invalid JSON data

## License

MIT License 