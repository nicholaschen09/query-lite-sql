document.addEventListener('DOMContentLoaded', function () {
    // DOM elements
    const sqlQueryTextarea = document.getElementById('sql-query');
    const runQueryButton = document.getElementById('run-query');
    const errorMessage = document.getElementById('error-message');
    const resultsHeader = document.getElementById('results-header');
    const resultsBody = document.getElementById('results-body');
    const historyBody = document.getElementById('history-body');

    // Load query history on page load
    loadQueryHistory();

    // Run query button event listener
    runQueryButton.addEventListener('click', function () {
        const query = sqlQueryTextarea.value.trim();
        if (!query) {
            showError('Please enter a SQL query');
            return;
        }

        executeQuery(query);
    });

    // Execute a SQL query
    function executeQuery(query) {
        clearError();

        fetch(`/api/query?q=${encodeURIComponent(query)}`)
            .then(response => response.json())
            .then(data => {
                if (data.error) {
                    showError(data.error);
                } else {
                    displayResults(data.result);
                    loadQueryHistory(); // Refresh history after successful query
                }
            })
            .catch(error => {
                showError('Error executing query: ' + error.message);
            });
    }

    // Load query history
    function loadQueryHistory() {
        fetch('/api/history')
            .then(response => response.json())
            .then(data => {
                displayHistory(data.history);
            })
            .catch(error => {
                console.error('Error loading history:', error);
            });
    }

    // Display query results
    function displayResults(results) {
        // Clear previous results
        resultsHeader.innerHTML = '';
        resultsBody.innerHTML = '';

        if (!Array.isArray(results) || results.length === 0) {
            resultsBody.innerHTML = '<tr><td>No results found</td></tr>';
            return;
        }

        // Create header row
        const headerRow = document.createElement('tr');
        const firstRow = results[0];
        const columns = Object.keys(firstRow);

        columns.forEach(column => {
            const th = document.createElement('th');
            th.textContent = column;
            headerRow.appendChild(th);
        });

        resultsHeader.appendChild(headerRow);

        // Create data rows
        results.forEach(row => {
            const tr = document.createElement('tr');

            columns.forEach(column => {
                const td = document.createElement('td');
                td.textContent = row[column] !== null && row[column] !== undefined ? row[column] : 'null';
                tr.appendChild(td);
            });

            resultsBody.appendChild(tr);
        });
    }

    // Display query history
    function displayHistory(history) {
        historyBody.innerHTML = '';

        if (!Array.isArray(history) || history.length === 0) {
            historyBody.innerHTML = '<tr><td colspan="5">No query history</td></tr>';
            return;
        }

        history.forEach(item => {
            const tr = document.createElement('tr');

            const idTd = document.createElement('td');
            idTd.textContent = item.id;
            tr.appendChild(idTd);

            const queryTd = document.createElement('td');
            queryTd.textContent = item.query;
            tr.appendChild(queryTd);

            const resultTd = document.createElement('td');
            resultTd.textContent = truncateText(item.result, 50);
            tr.appendChild(resultTd);

            const timestampTd = document.createElement('td');
            timestampTd.textContent = formatTimestamp(item.timestamp);
            tr.appendChild(timestampTd);

            const actionTd = document.createElement('td');
            const runAgainButton = document.createElement('button');
            runAgainButton.textContent = 'Run Again';
            runAgainButton.addEventListener('click', function () {
                sqlQueryTextarea.value = item.query;
                executeQuery(item.query);
            });
            actionTd.appendChild(runAgainButton);
            tr.appendChild(actionTd);

            historyBody.appendChild(tr);
        });
    }

    // Helper functions
    function showError(message) {
        errorMessage.textContent = message;
        errorMessage.style.display = 'block';
    }

    function clearError() {
        errorMessage.textContent = '';
        errorMessage.style.display = 'none';
    }

    function truncateText(text, maxLength) {
        if (text.length <= maxLength) return text;
        return text.substring(0, maxLength) + '...';
    }

    function formatTimestamp(timestamp) {
        const date = new Date(timestamp);
        return date.toLocaleString();
    }
}); 