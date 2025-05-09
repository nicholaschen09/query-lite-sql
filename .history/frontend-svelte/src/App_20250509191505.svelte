<script>
  import { onMount } from 'svelte';

  // State variables
  let query = "SELECT * FROM table LIMIT 10";
  let result = null;
  let error = null;
  let history = [];
  let loading = false;

  // Execute a SQL query
  async function executeQuery(queryText = query) {
    loading = true;
    error = null;
    
    try {
      const response = await fetch(`/api/query?q=${encodeURIComponent(queryText)}`);
      const data = await response.json();
      
      if (data.error) {
        error = data.error;
        result = null;
      } else {
        result = data;
        error = null;
      }
      
      // Refresh history after query execution
      loadHistory();
    } catch (err) {
      error = err.message;
      result = null;
    } finally {
      loading = false;
    }
  }

  // Load query history
  async function loadHistory() {
    try {
      const response = await fetch('/api/history');
      history = await response.json();
    } catch (err) {
      console.error('Error loading history:', err);
    }
  }

  // Format timestamp for display
  function formatTimestamp(timestamp) {
    return new Date(timestamp).toLocaleString();
  }

  // Load history on component mount
  onMount(() => {
    loadHistory();
  });
</script>

<main>
  <h1>Query Lite SQL</h1>
  
  <div class="query-section">
    <h2>SQL Query</h2>
    <textarea bind:value={query} placeholder="Enter your SQL query here..."></textarea>
    <button on:click={() => executeQuery()} disabled={loading}>
      {loading ? 'Executing...' : 'Execute Query'}
    </button>
  </div>
  
  <div class="result-section">
    <h2>Result</h2>
    {#if error}
      <div class="error">{error}</div>
    {:else if result}
      {#if Array.isArray(result) && result.length > 0}
        <table>
          <thead>
            <tr>
              {#each Object.keys(result[0]) as column}
                <th>{column}</th>
              {/each}
            </tr>
          </thead>
          <tbody>
            {#each result as row}
              <tr>
                {#each Object.keys(result[0]) as column}
                  <td>{row[column] !== null ? row[column] : 'null'}</td>
                {/each}
              </tr>
            {/each}
          </tbody>
        </table>
      {:else}
        <div>No results found or empty array.</div>
      {/if}
    {:else if loading}
      <div>Loading...</div>
    {:else}
      <div>Execute a query to see results</div>
    {/if}
  </div>
  
  <div class="history-section">
    <h2>Query History</h2>
    {#if history.length === 0}
      <div>No query history yet.</div>
    {:else}
      <div class="history-list">
        {#each history as item}
          <div class="history-item">
            <div class="timestamp">{formatTimestamp(item.timestamp)}</div>
            <div class="query-text">{item.query}</div>
            <button on:click={() => executeQuery(item.query)}>Run Again</button>
          </div>
        {/each}
      </div>
    {/if}
  </div>
</main>

<style>
  main {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
  }
  
  h1, h2 {
    color: #333;
  }
  
  textarea {
    width: 100%;
    height: 100px;
    padding: 8px;
    margin-bottom: 10px;
    font-family: monospace;
  }
  
  button {
    background-color: #4CAF50;
    color: white;
    padding: 10px 15px;
    border: none;
    cursor: pointer;
    font-size: 16px;
  }
  
  button:hover:not([disabled]) {
    background-color: #45a049;
  }
  
  button[disabled] {
    opacity: 0.6;
    cursor: not-allowed;
  }
  
  .result-section {
    margin-top: 20px;
    border: 1px solid #ddd;
    padding: 15px;
    background-color: #f9f9f9;
  }
  
  .error {
    color: red;
  }
  
  table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 10px;
  }
  
  th, td {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
  }
  
  th {
    background-color: #f2f2f2;
  }
  
  .history-item {
    border: 1px solid #ddd;
    padding: 10px;
    margin-bottom: 10px;
    background-color: #f9f9f9;
  }
  
  .query-text {
    font-family: monospace;
    background-color: #eee;
    padding: 5px;
    margin: 5px 0;
  }
  
  .timestamp {
    color: #777;
    font-size: 12px;
  }
  
  .history-section {
    margin-top: 20px;
  }
</style> 