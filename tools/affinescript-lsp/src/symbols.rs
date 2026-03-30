// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! Phase B: Symbol Table and References
//!
//! Parses the v2 `--json` contract's `symbols` and `references` fields.
//! Enables goto-definition, find-references, and rename.

use tower_lsp::lsp_types::*;

/// A symbol from the compiler's symbol table.
#[derive(Debug, Clone)]
pub struct CompilerSymbol {
    pub id: i64,
    pub name: String,
    pub kind: String,
    pub file: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
    pub type_info: Option<String>,
    pub quantity: Option<String>,
}

impl CompilerSymbol {
    /// Convert the definition location to an LSP Location.
    pub fn to_location(&self) -> Option<Location> {
        let uri = Url::from_file_path(&self.file).ok()?;
        Some(Location {
            uri,
            range: Range {
                start: Position {
                    line: self.start_line.saturating_sub(1),
                    character: self.start_col.saturating_sub(1),
                },
                end: Position {
                    line: self.end_line.saturating_sub(1),
                    character: self.end_col.saturating_sub(1),
                },
            },
        })
    }

    /// Convert to LSP SymbolKind.
    pub fn to_symbol_kind(&self) -> SymbolKind {
        match self.kind.as_str() {
            "function" => SymbolKind::FUNCTION,
            "variable" => SymbolKind::VARIABLE,
            "type" | "type_variable" => SymbolKind::TYPE_PARAMETER,
            "effect" | "effect_operation" => SymbolKind::EVENT,
            "trait" => SymbolKind::INTERFACE,
            "module" => SymbolKind::MODULE,
            "constructor" => SymbolKind::ENUM_MEMBER,
            _ => SymbolKind::VARIABLE,
        }
    }

    /// Format hover information for this symbol.
    pub fn hover_text(&self) -> String {
        let mut parts = vec![format!("**{}** ({})", self.name, self.kind)];
        if let Some(ty) = &self.type_info {
            parts.push(format!("Type: `{}`", ty));
        }
        if let Some(q) = &self.quantity {
            parts.push(format!("Quantity: {}", q));
        }
        parts.join("\n\n")
    }
}

/// A reference (use-site) for a symbol.
#[derive(Debug, Clone)]
pub struct CompilerReference {
    pub symbol_id: i64,
    pub file: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

impl CompilerReference {
    /// Convert to LSP Location.
    pub fn to_location(&self) -> Option<Location> {
        let uri = Url::from_file_path(&self.file).ok()?;
        Some(Location {
            uri,
            range: Range {
                start: Position {
                    line: self.start_line.saturating_sub(1),
                    character: self.start_col.saturating_sub(1),
                },
                end: Position {
                    line: self.end_line.saturating_sub(1),
                    character: self.end_col.saturating_sub(1),
                },
            },
        })
    }
}

/// Parsed v2 JSON output from the compiler.
#[derive(Debug, Default)]
pub struct CompilerOutput {
    pub symbols: Vec<CompilerSymbol>,
    pub references: Vec<CompilerReference>,
}

impl CompilerOutput {
    /// Parse symbols and references from v2 JSON output.
    pub fn from_json(json: &serde_json::Value) -> Self {
        let mut output = CompilerOutput::default();

        // Parse symbols array.
        if let Some(syms) = json.get("symbols").and_then(|v| v.as_array()) {
            for sym_val in syms {
                if let Some(sym) = parse_symbol(sym_val) {
                    output.symbols.push(sym);
                }
            }
        }

        // Parse references map: { "id": [ spans ] }
        if let Some(refs) = json.get("references").and_then(|v| v.as_object()) {
            for (id_str, spans_val) in refs {
                if let (Ok(sym_id), Some(spans)) = (id_str.parse::<i64>(), spans_val.as_array()) {
                    for span_val in spans {
                        if let Some(r) = parse_reference(sym_id, span_val) {
                            output.references.push(r);
                        }
                    }
                }
            }
        }

        output
    }

    /// Find the symbol whose definition span contains the given position.
    pub fn find_symbol_at(&self, file: &str, line: u32, col: u32) -> Option<&CompilerSymbol> {
        // First try exact name match at position (for goto-def on usage).
        // Then fall back to definition span match.
        self.symbols.iter().find(|sym| {
            sym.file == file
                && line >= sym.start_line && line <= sym.end_line
                && (line != sym.start_line || col >= sym.start_col)
                && (line != sym.end_line || col <= sym.end_col)
        })
    }

    /// Find a symbol by name (for goto-def when cursor is on a usage site).
    pub fn find_symbol_by_name(&self, name: &str) -> Option<&CompilerSymbol> {
        self.symbols.iter().find(|sym| sym.name == name)
    }

    /// Find all references for a symbol ID.
    pub fn find_references(&self, symbol_id: i64) -> Vec<&CompilerReference> {
        self.references.iter().filter(|r| r.symbol_id == symbol_id).collect()
    }
}

fn parse_symbol(val: &serde_json::Value) -> Option<CompilerSymbol> {
    Some(CompilerSymbol {
        id: val.get("id")?.as_i64()?,
        name: val.get("name")?.as_str()?.to_string(),
        kind: val.get("kind")?.as_str()?.to_string(),
        file: val.get("file")?.as_str()?.to_string(),
        start_line: val.get("start_line")?.as_u64()? as u32,
        start_col: val.get("start_col")?.as_u64()? as u32,
        end_line: val.get("end_line")?.as_u64()? as u32,
        end_col: val.get("end_col")?.as_u64()? as u32,
        type_info: val.get("type").and_then(|v| v.as_str()).map(|s| s.to_string()),
        quantity: val.get("quantity").and_then(|v| v.as_str()).map(|s| s.to_string()),
    })
}

fn parse_reference(symbol_id: i64, val: &serde_json::Value) -> Option<CompilerReference> {
    Some(CompilerReference {
        symbol_id,
        file: val.get("file")?.as_str()?.to_string(),
        start_line: val.get("start_line")?.as_u64()? as u32,
        start_col: val.get("start_col")?.as_u64()? as u32,
        end_line: val.get("end_line")?.as_u64()? as u32,
        end_col: val.get("end_col")?.as_u64()? as u32,
    })
}
