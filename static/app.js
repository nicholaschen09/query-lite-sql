document.addEventListener('DOMContentLoaded', () => {
    const queryInput = document.getElementById('queryInput');
    const executeButton = document.getElementById('executeQuery');
    const queryResult = document.getElementById('queryResult');
    const queryHistory = document.getElementById('queryHistory');

    // Load query history on page load
    loadQueryHistory();

    executeButton.addEventListener('click', async () => {
        const query = queryInput.value.trim();
        if (!query) return;

        try {
            const response = await fetch('/api/query', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ query }),
            });

            const data = await response.json();

            if (data.error) {
                displayError(data.error);
            } else {
                displayResult(data);
            }

            // Reload query history after successful query
            loadQueryHistory();
        } catch (error) {
            displayError('Failed to execute query: ' + error.message);
        }
    });

    async function loadQueryHistory() {
        try {
            const response = await fetch('/api/history');
            const history = await response.json();
            displayHistory(history);
        } catch (error) {
            console.error('Failed to load query history:', error);
        }
    }

    function displayResult(data) {
        if (Array.isArray(data)) {
            // Create table for array results
            const table = document.createElement('table');

            // Create header
            const thead = document.createElement('thead');
            const headerRow = document.createElement('tr');
            Object.keys(data[0]).forEach(key => {
                const th = document.createElement('th');
                th.textContent = key;
                headerRow.appendChild(th);
            });
            thead.appendChild(headerRow);
            table.appendChild(thead);

            // Create body
            const tbody = document.createElement('tbody');
            data.forEach(row => {
                const tr = document.createElement('tr');
                Object.values(row).forEach(value => {
                    const td = document.createElement('td');
                    td.textContent = value;
                    tr.appendChild(td);
                });
                tbody.appendChild(tr);
            });
            table.appendChild(tbody);

            queryResult.innerHTML = '';
            queryResult.appendChild(table);
        } else {
            // Display non-array results as JSON
            queryResult.textContent = JSON.stringify(data, null, 2);
        }
    }

    function displayError(error) {
        queryResult.innerHTML = `<div class="error">${error}</div>`;
    }

    function displayHistory(history) {
        queryHistory.innerHTML = '';

        if (history.length === 0) {
            queryHistory.textContent = 'No query history available.';
            return;
        }

        const table = document.createElement('table');

        // Create header
        const thead = document.createElement('thead');
        const headerRow = document.createElement('tr');
        ['Query', 'Result', 'Time'].forEach(header => {
            const th = document.createElement('th');
            th.textContent = header;
            headerRow.appendChild(th);
        });
        thead.appendChild(headerRow);
        table.appendChild(thead);

        // Create body
        const tbody = document.createElement('tbody');
        history.forEach(item => {
            const tr = document.createElement('tr');

            const queryTd = document.createElement('td');
            queryTd.textContent = item.query_text;
            tr.appendChild(queryTd);

            const resultTd = document.createElement('td');
            resultTd.textContent = item.query_result;
            tr.appendChild(resultTd);

            const timeTd = document.createElement('td');
            timeTd.textContent = new Date(item.query_time).toLocaleString();
            tr.appendChild(timeTd);

            tbody.appendChild(tr);
        });
        table.appendChild(tbody);

        queryHistory.appendChild(table);
    }
}); 