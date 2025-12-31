// SPDX-License-Identifier: Apache-2.0 OR MIT
// AffineScript Documentation Search

(function() {
  'use strict';

  // HTML escape function to prevent XSS
  function escapeHtml(text) {
    if (typeof text !== 'string') return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  // Safe URL validation
  function isValidUrl(url) {
    if (typeof url !== 'string') return false;
    // Only allow relative URLs or same-origin URLs
    try {
      if (url.startsWith('/') || url.startsWith('./') || url.startsWith('../')) {
        return true;
      }
      const parsed = new URL(url, window.location.origin);
      return parsed.origin === window.location.origin;
    } catch {
      return false;
    }
  }

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
        clearResults();
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
      const nameLower = (entry.name || '').toLowerCase();
      const pathLower = (entry.path || '').toLowerCase();

      if (nameLower === query) return 100;
      if (nameLower.startsWith(query)) return 50;
      if (nameLower.includes(query)) return 25;
      if (pathLower.includes(query)) return 10;

      return 0;
    }

    function clearResults() {
      while (searchResults.firstChild) {
        searchResults.removeChild(searchResults.firstChild);
      }
      searchResults.style.display = 'none';
    }

    function renderResults(results) {
      clearResults();

      if (results.length === 0) {
        const noResults = document.createElement('div');
        noResults.className = 'no-results';
        noResults.textContent = 'No results found';
        searchResults.appendChild(noResults);
        searchResults.style.display = 'block';
        return;
      }

      for (const r of results) {
        const link = document.createElement('a');
        link.className = 'search-result';

        // Validate URL before setting href
        const url = r.entry.url;
        if (isValidUrl(url)) {
          link.href = url;
        } else {
          link.href = '#';
        }

        const kindSpan = document.createElement('span');
        kindSpan.className = 'result-kind';
        kindSpan.textContent = r.entry.kind || '';
        link.appendChild(kindSpan);

        const nameSpan = document.createElement('span');
        nameSpan.className = 'result-name';
        nameSpan.textContent = r.entry.name || '';
        link.appendChild(nameSpan);

        const pathSpan = document.createElement('span');
        pathSpan.className = 'result-path';
        pathSpan.textContent = r.entry.path || '';
        link.appendChild(pathSpan);

        if (r.entry.description) {
          const descSpan = document.createElement('span');
          descSpan.className = 'result-desc';
          descSpan.textContent = r.entry.description;
          link.appendChild(descSpan);
        }

        searchResults.appendChild(link);
      }

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
