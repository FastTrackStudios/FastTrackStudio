// task-publish search — vanilla case-insensitive substring
// search over `/assets/search.json`. ~50 lines, no deps.
//
// Usage in HTML:
//   <input id="search-input" type="search" placeholder="Search…">
//   <ul id="search-results"></ul>
//   <script src="/assets/search.js" defer></script>

(async function () {
  const input = document.getElementById('search-input');
  const results = document.getElementById('search-results');
  if (!input || !results) return;

  let entries = [];
  try {
    const res = await fetch('/assets/search.json');
    entries = await res.json();
  } catch (e) {
    return; // no index — nothing to search
  }

  function render(filter) {
    const q = filter.trim().toLowerCase();
    results.innerHTML = '';
    if (!q) return;
    const hits = entries.filter(e =>
      e.title.toLowerCase().includes(q) ||
      e.snippet.toLowerCase().includes(q)
    ).slice(0, 20);
    for (const e of hits) {
      const li = document.createElement('li');
      const a = document.createElement('a');
      a.href = '/' + e.slug + '/';
      a.textContent = e.title;
      li.appendChild(a);
      const sn = document.createElement('span');
      sn.className = 'search-snippet';
      sn.textContent = e.snippet;
      li.appendChild(sn);
      results.appendChild(li);
    }
    if (hits.length === 0) {
      const li = document.createElement('li');
      li.textContent = 'no matches';
      li.className = 'search-empty';
      results.appendChild(li);
    }
  }

  input.addEventListener('input', () => render(input.value));
  // `/` keyboard shortcut to focus.
  document.addEventListener('keydown', (e) => {
    if (e.key === '/' && document.activeElement !== input) {
      e.preventDefault();
      input.focus();
    }
  });
})();
