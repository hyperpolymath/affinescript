// SPDX-License-Identifier: Apache-2.0 OR MIT
// AffineScript Documentation Search

(function() {
  'use strict';

  // Wait for DOM and search index to load
  document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search');
    if (!searchInput) return;

    let searchIndex = [];
    let searchResults = null;

    // Load search index
    if (window.searchIndex) {
      searchIndex = window.searchIndex;
    }

    // Create results container
    searchResults = document.createElement('div');
    searchResults.id = 'search-results';
    searchResults.className = 'search-results';
    searchInput.parentNode.appendChild(searchResults);

    // Search function
    function search(query) {
      if (!query || query.length < 2) {
        searchResults.innerHTML = '';
        searchResults.style.display = 'none';
        return;
      }

      const queryLower = query.toLowerCase();
      const results = [];

      for (const entry of searchIndex) {
        const score = computeScore(entry, queryLower);
        if (score > 0) {
          results.push({ entry, score });
        }
      }

      // Sort by score
      results.sort((a, b) => b.score - a.score);

      // Limit results
      const topResults = results.slice(0, 20);

      // Render results
      renderResults(topResults);
    }

    function computeScore(entry, query) {
      const nameLower = entry.name.toLowerCase();
      const pathLower = entry.path.toLowerCase();

      if (nameLower === query) return 100;
      if (nameLower.startsWith(query)) return 50;
      if (nameLower.includes(query)) return 25;
      if (pathLower.includes(query)) return 10;

      return 0;
    }

    function renderResults(results) {
      if (results.length === 0) {
        searchResults.innerHTML = '<div class="no-results">No results found</div>';
        searchResults.style.display = 'block';
        return;
      }

      const html = results.map(function(r) {
        return `
          <a class="search-result" href="${r.entry.url}">
            <span class="result-kind">${r.entry.kind}</span>
            <span class="result-name">${r.entry.name}</span>
            <span class="result-path">${r.entry.path}</span>
            ${r.entry.description ? `<span class="result-desc">${r.entry.description}</span>` : ''}
          </a>
        `;
      }).join('');

      searchResults.innerHTML = html;
      searchResults.style.display = 'block';
    }

    // Debounce search input
    let debounceTimer;
    searchInput.addEventListener('input', function() {
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(function() {
        search(searchInput.value);
      }, 150);
    });

    // Close results on outside click
    document.addEventListener('click', function(e) {
      if (!searchInput.contains(e.target) && !searchResults.contains(e.target)) {
        searchResults.style.display = 'none';
      }
    });

    // Keyboard navigation
    searchInput.addEventListener('keydown', function(e) {
      const results = searchResults.querySelectorAll('.search-result');
      const active = searchResults.querySelector('.search-result.active');
      let index = Array.from(results).indexOf(active);

      if (e.key === 'ArrowDown') {
        e.preventDefault();
        if (active) active.classList.remove('active');
        index = (index + 1) % results.length;
        if (results[index]) results[index].classList.add('active');
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        if (active) active.classList.remove('active');
        index = (index - 1 + results.length) % results.length;
        if (results[index]) results[index].classList.add('active');
      } else if (e.key === 'Enter') {
        if (active) {
          window.location.href = active.href;
        }
      } else if (e.key === 'Escape') {
        searchResults.style.display = 'none';
        searchInput.blur();
      }
    });

    // Focus search with /
    document.addEventListener('keydown', function(e) {
      if (e.key === '/' && document.activeElement !== searchInput) {
        e.preventDefault();
        searchInput.focus();
      }
    });
  });
})();
