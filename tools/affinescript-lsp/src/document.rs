// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Document Management
//!
//! Tracks open documents and their state.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tower_lsp::lsp_types::*;

/// Manages open documents
#[derive(Debug)]
pub struct DocumentManager {
    /// Open documents by URI
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

impl DocumentManager {
    /// Create a new document manager
    pub fn new() -> Self {
        DocumentManager {
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Open a document
    pub fn open(&self, uri: Url, text: String, version: i32) {
        let doc = Document::new(text, version);
        self.documents.write().unwrap().insert(uri, doc);
    }

    /// Close a document
    pub fn close(&self, uri: &Url) {
        self.documents.write().unwrap().remove(uri);
    }

    /// Apply changes to a document
    pub fn apply_changes(&self, uri: &Url, version: i32, changes: Vec<TextDocumentContentChangeEvent>) {
        let mut docs = self.documents.write().unwrap();
        if let Some(doc) = docs.get_mut(uri) {
            doc.apply_changes(version, changes);
        }
    }

    /// Get document text
    pub fn get_text(&self, uri: &Url) -> Option<String> {
        self.documents.read().unwrap().get(uri).map(|d| d.text.clone())
    }

    /// Get document version
    pub fn get_version(&self, uri: &Url) -> Option<i32> {
        self.documents.read().unwrap().get(uri).map(|d| d.version)
    }
}

impl Default for DocumentManager {
    fn default() -> Self {
        Self::new()
    }
}

/// A single document
#[derive(Debug)]
pub struct Document {
    /// Document text
    pub text: String,
    /// Document version
    pub version: i32,
    /// Line offsets (byte offset of each line start)
    line_offsets: Vec<usize>,
    // TODO: Add parsed AST cache
    // TODO: Add type-checked state cache
}

impl Document {
    /// Create a new document
    pub fn new(text: String, version: i32) -> Self {
        let line_offsets = compute_line_offsets(&text);
        Document {
            text,
            version,
            line_offsets,
        }
    }

    /// Apply content changes
    pub fn apply_changes(&mut self, version: i32, changes: Vec<TextDocumentContentChangeEvent>) {
        self.version = version;

        for change in changes {
            match change.range {
                Some(range) => {
                    // Incremental change
                    let start_offset = self.offset_at(range.start);
                    let end_offset = self.offset_at(range.end);
                    self.text.replace_range(start_offset..end_offset, &change.text);
                }
                None => {
                    // Full document change
                    self.text = change.text;
                }
            }
        }

        // Recompute line offsets
        self.line_offsets = compute_line_offsets(&self.text);
    }

    /// Get byte offset from position
    pub fn offset_at(&self, pos: Position) -> usize {
        let line = pos.line as usize;
        if line >= self.line_offsets.len() {
            return self.text.len();
        }

        let line_start = self.line_offsets[line];
        let line_end = if line + 1 < self.line_offsets.len() {
            self.line_offsets[line + 1]
        } else {
            self.text.len()
        };

        let col = pos.character as usize;
        let line_text = &self.text[line_start..line_end];

        // Handle UTF-16 code units
        let mut char_offset = 0;
        let mut utf16_offset = 0;

        for c in line_text.chars() {
            if utf16_offset >= col {
                break;
            }
            char_offset += c.len_utf8();
            utf16_offset += c.len_utf16();
        }

        line_start + char_offset
    }

    /// Get position from byte offset
    pub fn position_at(&self, offset: usize) -> Position {
        let offset = offset.min(self.text.len());

        // Find line
        let line = self
            .line_offsets
            .iter()
            .position(|&o| o > offset)
            .map(|l| l - 1)
            .unwrap_or(self.line_offsets.len() - 1);

        let line_start = self.line_offsets[line];
        let line_text = &self.text[line_start..offset];

        // Count UTF-16 code units
        let character = line_text.chars().map(|c| c.len_utf16()).sum::<usize>();

        Position {
            line: line as u32,
            character: character as u32,
        }
    }
}

/// Compute byte offsets of line starts
fn compute_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, c) in text.char_indices() {
        if c == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

// TODO: Phase 8 implementation
// - [ ] Add AST caching
// - [ ] Add incremental parsing
// - [ ] Add type information caching
// - [ ] Track diagnostics per document
// - [ ] Implement dependency tracking
