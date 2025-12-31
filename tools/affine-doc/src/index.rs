// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Search Index
//!
//! Builds and queries the documentation search index.

use crate::extract::{ItemDoc, ItemKind, ModuleDoc};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Search index entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchEntry {
    /// Entry name
    pub name: String,

    /// Full path (e.g., "std::vec::Vec::push")
    pub path: String,

    /// Item kind
    pub kind: String,

    /// Brief description (first line of doc)
    pub description: String,

    /// URL to documentation page
    pub url: String,
}

/// Search index builder
pub struct IndexBuilder {
    entries: Vec<SearchEntry>,
}

impl IndexBuilder {
    /// Create a new index builder
    pub fn new() -> Self {
        IndexBuilder {
            entries: Vec::new(),
        }
    }

    /// Add a module to the index
    pub fn add_module(&mut self, module: &ModuleDoc) {
        let module_path = module.path.join("::");

        // Add module itself
        self.entries.push(SearchEntry {
            name: module.path.last().cloned().unwrap_or_default(),
            path: module_path.clone(),
            kind: "Module".to_string(),
            description: first_line(module.doc.as_deref().unwrap_or("")),
            url: format!("{}.html", module_path.replace("::", "/")),
        });

        // Add items
        for item in &module.items {
            self.add_item(&module_path, item);
        }
    }

    /// Add an item to the index
    fn add_item(&mut self, module_path: &str, item: &ItemDoc) {
        let full_path = if module_path.is_empty() {
            item.name.clone()
        } else {
            format!("{}::{}", module_path, item.name)
        };

        self.entries.push(SearchEntry {
            name: item.name.clone(),
            path: full_path,
            kind: item.kind.display_name().to_string(),
            description: first_line(item.doc.as_deref().unwrap_or("")),
            url: format!(
                "{}.html#{}",
                module_path.replace("::", "/"),
                item.name
            ),
        });
    }

    /// Build the JSON search index
    pub fn build_json(&self) -> String {
        serde_json::to_string(&self.entries).unwrap_or_else(|_| "[]".to_string())
    }

    /// Write the search index to disk
    pub fn write(&self, output_dir: impl AsRef<Path>) -> anyhow::Result<()> {
        let json = self.build_json();
        std::fs::write(output_dir.as_ref().join("search-index.js"), format!(
            "window.searchIndex = {};",
            json
        ))?;
        Ok(())
    }
}

impl Default for IndexBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Search query
pub struct SearchQuery {
    /// Query text
    pub query: String,

    /// Filter by kind
    pub kind_filter: Option<ItemKind>,

    /// Maximum results
    pub limit: usize,
}

impl Default for SearchQuery {
    fn default() -> Self {
        SearchQuery {
            query: String::new(),
            kind_filter: None,
            limit: 50,
        }
    }
}

/// Search result
#[derive(Debug, Clone)]
pub struct SearchResult {
    /// Matching entry
    pub entry: SearchEntry,

    /// Relevance score
    pub score: f32,
}

/// Simple in-memory search (for client-side)
pub fn search(entries: &[SearchEntry], query: &SearchQuery) -> Vec<SearchResult> {
    let query_lower = query.query.to_lowercase();

    let mut results: Vec<SearchResult> = entries
        .iter()
        .filter_map(|entry| {
            // Apply kind filter
            if let Some(kind) = &query.kind_filter {
                if entry.kind != kind.display_name() {
                    return None;
                }
            }

            // Score matching
            let score = compute_score(&entry.name, &entry.path, &query_lower);
            if score > 0.0 {
                Some(SearchResult {
                    entry: entry.clone(),
                    score,
                })
            } else {
                None
            }
        })
        .collect();

    // Sort by score (descending)
    results.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());

    // Apply limit
    results.truncate(query.limit);

    results
}

/// Compute search relevance score
fn compute_score(name: &str, path: &str, query: &str) -> f32 {
    let name_lower = name.to_lowercase();
    let path_lower = path.to_lowercase();

    let mut score = 0.0;

    // Exact name match
    if name_lower == query {
        score += 100.0;
    }
    // Name starts with query
    else if name_lower.starts_with(query) {
        score += 50.0;
    }
    // Name contains query
    else if name_lower.contains(query) {
        score += 25.0;
    }
    // Path contains query
    else if path_lower.contains(query) {
        score += 10.0;
    }

    // Bonus for shorter names (more specific)
    if score > 0.0 {
        score += 10.0 / (name.len() as f32);
    }

    score
}

/// Get first line of text
fn first_line(text: &str) -> String {
    text.lines()
        .next()
        .unwrap_or("")
        .trim()
        .to_string()
}

// TODO: Phase 8 implementation
// - [ ] Use Tantivy for full-text search
// - [ ] Add fuzzy matching
// - [ ] Add type signature search
// - [ ] Add effect search
// - [ ] Cache search index
