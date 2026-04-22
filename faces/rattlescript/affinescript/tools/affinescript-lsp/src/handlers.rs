// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! Request Handlers
//!
//! Implementation of LSP request handlers for AffineScript.
//!
//! ## Phase C: go-to-definition, find-references, rename
//!
//! These features operate in two tiers:
//!
//! 1. **Compiler-backed** — When the AffineScript compiler emits v2 JSON with
//!    `symbols` and `references` arrays, the handlers use `CompilerOutput` for
//!    accurate, scope-aware results.
//!
//! 2. **Text-index fallback** — When compiler output is empty (e.g. the
//!    compiler binary is older or unavailable), the handlers fall back to
//!    `TextIndex`, which scans source text for definition keywords and
//!    identifier occurrences.  This is less precise (no scope resolution)
//!    but still returns useful results for single-file editing.

use std::collections::HashMap;
use tower_lsp::lsp_types::*;

use crate::text_index::{self, TextIndex};

/// Handle hover request — Phase B enhanced.
///
/// First checks the compiler's symbol table for type information.
/// Falls back to keyword descriptions for language constructs.
pub fn hover(
    uri: &Url,
    position: Position,
    text: &str,
    compiler_output: &crate::symbols::CompilerOutput,
) -> Option<Hover> {
    let word = get_word_at_position(text, position)?;

    // Phase B: try symbol table first for accurate type information.
    if let Some(sym) = compiler_output.find_symbol_by_name(&word) {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: sym.hover_text(),
            }),
            range: Some(Range {
                start: position,
                end: Position {
                    line: position.line,
                    character: position.character + utf16_len(&word),
                },
            }),
        });
    }

    // Fall back to keyword hover for language constructs.
    let hover_text = match word.as_str() {
        "fn" => "Defines a function",
        "let" => "Binds a value to a variable",
        "type" => "Defines a type alias",
        "struct" => "Defines a structure type",
        "enum" => "Defines an enumeration type",
        "effect" => "Defines an effect type",
        "handler" => "Defines an effect handler",
        "linear" => "Linear type qualifier (must be used exactly once)",
        "affine" => "Affine type qualifier (must be used at most once)",
        "unrestricted" => "Unrestricted type qualifier (can be used any number of times)",
        "borrow" => "Creates a temporary borrow of a value",
        "move" => "Transfers ownership of a value",
        _ => return None,
    };

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**{}**\n\n{}", word, hover_text),
        }),
        range: Some(Range {
            start: position,
            end: Position {
                line: position.line,
                character: position.character + utf16_len(&word),
            },
        }),
    })
}

/// Handle goto definition — Phase B + Phase C fallback.
///
/// Looks up the word at the cursor position in the compiler's symbol table.
/// If the compiler hasn't provided symbols (v1 JSON or unavailable), falls
/// back to the text-based index which scans for `fn`, `let`, `type`, etc.
pub fn goto_definition(
    uri: &Url,
    position: Position,
    text: &str,
    compiler_output: &crate::symbols::CompilerOutput,
) -> Option<Location> {
    let word = get_word_at_position(text, position)?;

    // Tier 1: compiler-backed symbol table.
    if let Some(sym) = compiler_output.find_symbol_by_name(&word) {
        return sym.to_location();
    }

    // Tier 2: text-index fallback for single-file go-to-definition.
    let index = TextIndex::build(text);
    let def = index.find_definition(&word)?;
    Some(Location {
        uri: uri.clone(),
        range: Range {
            start: Position { line: def.line, character: def.col_start },
            end: Position { line: def.line, character: def.col_end },
        },
    })
}

/// Handle find references — Phase C.
///
/// Finds the symbol at the cursor position, then returns all reference
/// locations.  Uses the compiler's reference index when available, otherwise
/// falls back to text-index scanning.
pub fn find_references(
    uri: &Url,
    position: Position,
    text: &str,
    include_declaration: bool,
    compiler_output: &crate::symbols::CompilerOutput,
) -> Vec<Location> {
    let word = match get_word_at_position(text, position) {
        Some(w) => w,
        None => return vec![],
    };

    // Tier 1: compiler-backed reference index.
    if let Some(sym) = compiler_output.find_symbol_by_name(&word) {
        let refs = compiler_output.find_references(sym.id);
        if !refs.is_empty() || compiler_output.symbols.len() > 0 {
            let mut locations: Vec<Location> = refs
                .into_iter()
                .filter_map(|r| r.to_location())
                .collect();

            if include_declaration {
                if let Some(def_loc) = sym.to_location() {
                    locations.insert(0, def_loc);
                }
            }

            return locations;
        }
    }

    // Tier 2: text-index fallback.
    let index = TextIndex::build(text);
    if index.find_definition(&word).is_none() && index.find_occurrences(&word).is_empty() {
        return vec![];
    }

    let occurrences = if include_declaration {
        index.find_occurrences(&word)
    } else {
        index.find_usages_only(&word)
    };

    occurrences
        .into_iter()
        .map(|occ| Location {
            uri: uri.clone(),
            range: Range {
                start: Position { line: occ.line, character: occ.col_start },
                end: Position { line: occ.line, character: occ.col_end },
            },
        })
        .collect()
}

/// Handle completion
pub fn completion(_uri: &Url, position: Position, text: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Get current line to determine context
    let line = match get_line_at_position(text, position.line) {
        Some(l) => l,
        None => return items,
    };

    let col = utf16_col_to_byte_offset(line, position.character as usize);
    let prefix = if col > 0 && col <= line.len() {
        &line[..col]
    } else {
        ""
    };

    // Keywords
    let keywords = vec![
        ("fn", "Function definition", "fn ${1:name}(${2:args}) -> ${3:Type} {\n\t$0\n}"),
        ("let", "Variable binding", "let ${1:name} = $0"),
        ("type", "Type alias", "type ${1:Name} = $0"),
        ("struct", "Structure type", "struct ${1:Name} {\n\t$0\n}"),
        ("enum", "Enumeration type", "enum ${1:Name} {\n\t$0\n}"),
        ("effect", "Effect type", "effect ${1:Name} {\n\t$0\n}"),
        ("handler", "Effect handler", "handler ${1:name} {\n\t$0\n}"),
        ("match", "Pattern matching", "match ${1:expr} {\n\t$0\n}"),
        ("if", "Conditional", "if ${1:condition} {\n\t$0\n}"),
        ("else", "Else clause", "else {\n\t$0\n}"),
        ("for", "For loop", "for ${1:var} in ${2:expr} {\n\t$0\n}"),
        ("while", "While loop", "while ${1:condition} {\n\t$0\n}"),
        ("return", "Return statement", "return $0"),
        ("linear", "Linear type qualifier", "linear $0"),
        ("affine", "Affine type qualifier", "affine $0"),
        ("unrestricted", "Unrestricted type qualifier", "unrestricted $0"),
        ("borrow", "Borrow expression", "borrow $0"),
        ("move", "Move expression", "move $0"),
    ];

    for (keyword, detail, snippet) in keywords {
        if keyword.starts_with(&prefix.trim_start()) || prefix.trim().is_empty() {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some(detail.to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }
    }

    // Standard library types
    let std_types = vec![
        ("Int", "Integer type"),
        ("Float", "Floating-point type"),
        ("Bool", "Boolean type"),
        ("String", "String type"),
        ("Unit", "Unit type"),
        ("List", "List type"),
        ("Option", "Optional type"),
        ("Result", "Result type"),
    ];

    for (type_name, detail) in std_types {
        items.push(CompletionItem {
            label: type_name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    items
}

/// Handle prepare-rename — Phase C.
///
/// Checks whether the symbol at the cursor position is renameable (i.e. it is
/// a user-defined identifier, not a keyword or builtin type).  Returns the
/// range of the symbol name so the editor can highlight it in the rename UI.
///
/// Uses the compiler symbol table when available, otherwise the text index.
pub fn prepare_rename(
    uri: &Url,
    position: Position,
    text: &str,
    compiler_output: &crate::symbols::CompilerOutput,
) -> Option<PrepareRenameResponse> {
    let word = get_word_at_position(text, position)?;

    // Keywords and builtins are not renameable.
    if !text_index::is_renameable(&word) {
        return None;
    }

    // Tier 1: check compiler symbol table.
    if let Some(sym) = compiler_output.find_symbol_by_name(&word) {
        // The symbol exists in the compiler output — it's renameable.
        let range = Range {
            start: Position {
                line: sym.start_line.saturating_sub(1),
                character: sym.start_col.saturating_sub(1),
            },
            end: Position {
                line: sym.end_line.saturating_sub(1),
                character: sym.end_col.saturating_sub(1),
            },
        };
        return Some(PrepareRenameResponse::Range(range));
    }

    // Tier 2: text-index fallback — find the identifier at the cursor.
    let index = TextIndex::build(text);
    let occ = index.find_at_position(position.line, position.character)?;

    // Only allow rename if this name has a definition in the file.
    // (Otherwise we'd be renaming something we can't find the source of.)
    if index.find_definition(&occ.name).is_some() || index.find_occurrences(&occ.name).len() > 1 {
        Some(PrepareRenameResponse::Range(Range {
            start: Position { line: occ.line, character: occ.col_start },
            end: Position { line: occ.line, character: occ.col_end },
        }))
    } else {
        None
    }
}

/// Handle rename — Phase C.
///
/// Finds all occurrences of the symbol at the cursor position and produces
/// a `WorkspaceEdit` that replaces every occurrence with the new name.
///
/// When the compiler provides v2 JSON output with cross-file references,
/// the rename can span multiple files.  In text-index fallback mode, the
/// rename is limited to the current document.
pub fn rename(
    uri: &Url,
    position: Position,
    new_name: &str,
    text: &str,
    compiler_output: &crate::symbols::CompilerOutput,
) -> Option<WorkspaceEdit> {
    let word = get_word_at_position(text, position)?;

    // Validate the new name is a legal identifier.
    if !text_index::is_valid_identifier(new_name) {
        return None;
    }

    // Cannot rename keywords or builtins.
    if !text_index::is_renameable(&word) {
        return None;
    }

    // Tier 1: compiler-backed rename (supports cross-file).
    if let Some(sym) = compiler_output.find_symbol_by_name(&word) {
        let refs = compiler_output.find_references(sym.id);
        if !refs.is_empty() || compiler_output.symbols.len() > 0 {
            let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

            // Add the definition site.
            if let Some(def_loc) = sym.to_location() {
                changes
                    .entry(def_loc.uri.clone())
                    .or_default()
                    .push(TextEdit {
                        range: def_loc.range,
                        new_text: new_name.to_string(),
                    });
            }

            // Add all reference sites.
            for r in &refs {
                if let Some(ref_loc) = r.to_location() {
                    changes
                        .entry(ref_loc.uri.clone())
                        .or_default()
                        .push(TextEdit {
                            range: ref_loc.range,
                            new_text: new_name.to_string(),
                        });
                }
            }

            return Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            });
        }
    }

    // Tier 2: text-index fallback (single file only).
    let index = TextIndex::build(text);
    let occurrences = index.find_occurrences(&word);
    if occurrences.is_empty() {
        return None;
    }

    let edits: Vec<TextEdit> = occurrences
        .into_iter()
        .map(|occ| TextEdit {
            range: Range {
                start: Position { line: occ.line, character: occ.col_start },
                end: Position { line: occ.line, character: occ.col_end },
            },
            new_text: new_name.to_string(),
        })
        .collect();

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

/// Handle document formatting
pub fn format(uri: &Url, text: &str, options: &FormattingOptions) -> Vec<TextEdit> {
    // Basic indentation-based formatting
    let lines: Vec<&str> = text.lines().collect();
    let mut formatted_lines = Vec::new();
    let mut indent_level: usize = 0;
    let indent_str = if options.insert_spaces {
        " ".repeat(options.tab_size as usize)
    } else {
        "\t".to_string()
    };

    let line_count = lines.len();
    for line in lines {
        let trimmed = line.trim();

        // Decrease indent for closing braces
        if trimmed.starts_with('}') || trimmed.starts_with(']') || trimmed.starts_with(')') {
            indent_level = indent_level.saturating_sub(1);
        }

        // Add indented line
        if !trimmed.is_empty() {
            formatted_lines.push(format!("{}{}", indent_str.repeat(indent_level), trimmed));
        } else {
            formatted_lines.push(String::new());
        }

        // Increase indent for opening braces
        if trimmed.ends_with('{') || trimmed.ends_with('[') || trimmed.ends_with('(') {
            indent_level += 1;
        }
    }

    let formatted_text = formatted_lines.join("\n");

    if formatted_text == text {
        return vec![];
    }

    vec![TextEdit {
        range: Range {
            start: Position { line: 0, character: 0 },
            end: Position {
                line: line_count as u32,
                character: 0,
            },
        },
        new_text: formatted_text,
    }]
}

/// Handle code actions
pub fn code_actions(_uri: &Url, _range: Range, _diagnostics: &[Diagnostic]) -> Vec<CodeAction> {
    // Phase D: requires fix suggestions in --json output
    vec![]
}

/// Handle document symbols
pub fn document_symbols(_uri: &Url, text: &str) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    let lines: Vec<&str> = text.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        // Byte offset of where trimmed content starts within the original line
        let leading_bytes = line.len() - trimmed.len();
        let leading_utf16 = utf16_len(&line[..leading_bytes]);

        // Match function definitions
        if trimmed.starts_with("fn ") {
            if let Some(name_start) = trimmed.find("fn ").map(|i| i + 3) {
                if let Some(name_end) = trimmed[name_start..].find(|c: char| c == '(' || c.is_whitespace()) {
                    let name = &trimmed[name_start..name_start + name_end];
                    let name_start_utf16 = leading_utf16 + utf16_len(&trimmed[..name_start]);
                    let name_end_utf16 = name_start_utf16 + utf16_len(name);
                    symbols.push(DocumentSymbol {
                        name: name.to_string(),
                        detail: Some(trimmed.to_string()),
                        kind: SymbolKind::FUNCTION,
                        range: Range {
                            start: Position { line: line_num as u32, character: 0 },
                            end: Position { line: line_num as u32, character: utf16_len(line) },
                        },
                        selection_range: Range {
                            start: Position { line: line_num as u32, character: name_start_utf16 },
                            end: Position { line: line_num as u32, character: name_end_utf16 },
                        },
                        children: None,
                        tags: None,
                        deprecated: None,
                    });
                }
            }
        }

        // Match type definitions
        if trimmed.starts_with("type ") || trimmed.starts_with("struct ") || trimmed.starts_with("enum ") {
            let keyword_len = if trimmed.starts_with("type ") { 5 } else if trimmed.starts_with("struct ") { 7 } else { 5 };
            if let Some(name_end) = trimmed[keyword_len..].find(|c: char| c == '=' || c == '{' || c.is_whitespace()) {
                let name = trimmed[keyword_len..keyword_len + name_end].trim();
                let name_start_utf16 = leading_utf16 + utf16_len(&trimmed[..keyword_len]);
                let name_end_utf16 = name_start_utf16 + utf16_len(name);
                symbols.push(DocumentSymbol {
                    name: name.to_string(),
                    detail: Some(trimmed.to_string()),
                    kind: SymbolKind::STRUCT,
                    range: Range {
                        start: Position { line: line_num as u32, character: 0 },
                        end: Position { line: line_num as u32, character: utf16_len(line) },
                    },
                    selection_range: Range {
                        start: Position { line: line_num as u32, character: name_start_utf16 },
                        end: Position { line: line_num as u32, character: name_end_utf16 },
                    },
                    children: None,
                    tags: None,
                    deprecated: None,
                });
            }
        }
    }

    symbols
}

/// Handle signature help
pub fn signature_help(_uri: &Url, _position: Position, _text: &str) -> Option<SignatureHelp> {
    // Phase B: requires function signature data in --json output
    None
}

/// Handle inlay hints
pub fn inlay_hints(_uri: &Url, _range: Range, _text: &str) -> Vec<InlayHint> {
    // Phase D: requires inferred type data in --json output
    vec![]
}

/// Convert a UTF-16 column offset to a byte offset within a line.
/// LSP positions use UTF-16 code units; Rust strings are UTF-8.
fn utf16_col_to_byte_offset(line: &str, utf16_col: usize) -> usize {
    let mut utf16_offset = 0;
    let mut byte_offset = 0;
    for c in line.chars() {
        if utf16_offset >= utf16_col {
            break;
        }
        utf16_offset += c.len_utf16();
        byte_offset += c.len_utf8();
    }
    byte_offset
}

/// Count UTF-16 code units in a string (for converting byte lengths to LSP positions).
fn utf16_len(s: &str) -> u32 {
    s.chars().map(|c| c.len_utf16()).sum::<usize>() as u32
}

/// Get word at position in text
fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = utf16_col_to_byte_offset(line, position.character as usize);

    if col >= line.len() {
        return None;
    }

    // Find word boundaries (byte offsets within line)
    let start = line[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let end = line[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| col + i)
        .unwrap_or(line.len());

    if start >= end {
        return None;
    }

    Some(line[start..end].to_string())
}

/// Get line at position
fn get_line_at_position(text: &str, line_num: u32) -> Option<&str> {
    text.lines().nth(line_num as usize)
}

// Phase B: semantic tokens, call hierarchy, type hierarchy (requires compiler integration)
// Phase D: caching for performance (once --json output includes enough data)

// ---------------------------------------------------------------------------
// Tests — Phase C handler integration tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::CompilerOutput;

    /// Helper: create a file URI from a dummy path.
    fn test_uri() -> Url {
        Url::parse("file:///tmp/test.affine").unwrap()
    }

    /// Helper: empty compiler output (simulates no v2 JSON available).
    fn empty_co() -> CompilerOutput {
        CompilerOutput::default()
    }

    // -- goto_definition (text-index fallback) --

    #[test]
    fn goto_definition_finds_function() {
        let src = "fn greet(name: String) -> Unit {}\nlet x = greet(\"hi\")";
        let uri = test_uri();
        let co = empty_co();
        // Position on "greet" in the second line (col 8 = "g" of "greet").
        let pos = Position { line: 1, character: 8 };
        let result = goto_definition(&uri, pos, src, &co);
        assert!(result.is_some(), "goto_definition should find 'greet'");
        let loc = result.unwrap();
        assert_eq!(loc.range.start.line, 0, "definition should be on line 0");
        assert_eq!(loc.range.start.character, 3, "definition should start at col 3 (after 'fn ')");
    }

    #[test]
    fn goto_definition_finds_variable() {
        let src = "let counter = 0\nlet y = counter + 1";
        let uri = test_uri();
        let co = empty_co();
        // Position on "counter" in the second line.
        let pos = Position { line: 1, character: 8 };
        let result = goto_definition(&uri, pos, src, &co);
        assert!(result.is_some(), "goto_definition should find 'counter'");
        let loc = result.unwrap();
        assert_eq!(loc.range.start.line, 0);
    }

    #[test]
    fn goto_definition_returns_none_for_keyword() {
        let src = "fn greet() -> Unit {}";
        let uri = test_uri();
        let co = empty_co();
        // Position on "fn" keyword.
        let pos = Position { line: 0, character: 0 };
        let result = goto_definition(&uri, pos, src, &co);
        // "fn" is a keyword — no user-defined definition for it.
        // The text index won't have a definition named "fn" in its definitions list
        // (definitions extract the name AFTER the keyword).
        assert!(result.is_none());
    }

    // -- find_references (text-index fallback) --

    #[test]
    fn find_references_returns_all_occurrences() {
        let src = "let x = 1\nlet y = x + 2\nlet z = x * 3";
        let uri = test_uri();
        let co = empty_co();
        // Position on "x" in the first line.
        let pos = Position { line: 0, character: 4 };
        let refs = find_references(&uri, pos, src, true, &co);
        // x appears 3 times: definition + 2 usages.
        assert_eq!(refs.len(), 3, "should find 3 references to 'x'");
    }

    #[test]
    fn find_references_excludes_declaration() {
        let src = "let x = 1\nlet y = x + 2\nlet z = x * 3";
        let uri = test_uri();
        let co = empty_co();
        let pos = Position { line: 0, character: 4 };
        let refs = find_references(&uri, pos, src, false, &co);
        // x usages only (not the definition).
        assert_eq!(refs.len(), 2, "should find 2 usage references to 'x'");
    }

    // -- prepare_rename --

    #[test]
    fn prepare_rename_accepts_user_symbol() {
        let src = "fn greet() -> Unit {}";
        let uri = test_uri();
        let co = empty_co();
        // Position on "greet".
        let pos = Position { line: 0, character: 4 };
        let result = prepare_rename(&uri, pos, src, &co);
        assert!(result.is_some(), "should allow renaming 'greet'");
    }

    #[test]
    fn prepare_rename_rejects_keyword() {
        let src = "fn greet() -> Unit {}";
        let uri = test_uri();
        let co = empty_co();
        // Position on "fn".
        let pos = Position { line: 0, character: 0 };
        let result = prepare_rename(&uri, pos, src, &co);
        assert!(result.is_none(), "should not allow renaming 'fn'");
    }

    // -- rename --

    #[test]
    fn rename_replaces_all_occurrences() {
        let src = "let x = 1\nlet y = x + 2\nlet z = x * 3";
        let uri = test_uri();
        let co = empty_co();
        let pos = Position { line: 0, character: 4 };
        let result = rename(&uri, pos, "count", src, &co);
        assert!(result.is_some(), "rename should produce a WorkspaceEdit");
        let edit = result.unwrap();
        let changes = edit.changes.unwrap();
        let edits = changes.get(&uri).expect("should have edits for the test URI");
        assert_eq!(edits.len(), 3, "should replace all 3 occurrences of 'x'");
        for e in edits {
            assert_eq!(e.new_text, "count");
        }
    }

    #[test]
    fn rename_rejects_invalid_new_name() {
        let src = "let x = 1";
        let uri = test_uri();
        let co = empty_co();
        let pos = Position { line: 0, character: 4 };
        // "123" is not a valid identifier.
        let result = rename(&uri, pos, "123", src, &co);
        assert!(result.is_none(), "should reject invalid identifier '123'");
    }

    #[test]
    fn rename_rejects_keyword_as_new_name() {
        let src = "let x = 1";
        let uri = test_uri();
        let co = empty_co();
        let pos = Position { line: 0, character: 4 };
        // "fn" is a keyword.
        let result = rename(&uri, pos, "fn", src, &co);
        assert!(result.is_none(), "should reject keyword 'fn' as new name");
    }

    // -- compiler-backed tests (v2 JSON symbols) --

    #[test]
    fn goto_definition_uses_compiler_output_when_available() {
        let json_str = r#"{
            "version": 2,
            "diagnostics": [],
            "success": true,
            "symbols": [
                {
                    "id": 1,
                    "name": "add",
                    "kind": "function",
                    "file": "/tmp/test.affine",
                    "start_line": 1,
                    "start_col": 4,
                    "end_line": 1,
                    "end_col": 7,
                    "type": "(Int, Int) -> Int"
                }
            ],
            "references": {
                "1": [
                    {"file": "/tmp/test.affine", "start_line": 5, "start_col": 10, "end_line": 5, "end_col": 13}
                ]
            }
        }"#;
        let json: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let co = crate::symbols::CompilerOutput::from_json(&json);

        let src = "fn add(a: Int, b: Int) -> Int { a + b }\n\nlet result = add(1, 2)";
        let uri = test_uri();
        // Position on "add" in the usage site.
        let pos = Position { line: 2, character: 14 };
        let result = goto_definition(&uri, pos, src, &co);
        assert!(result.is_some(), "should find compiler-backed definition");
        let loc = result.unwrap();
        // Compiler reports 1-based line/col; to_location converts to 0-based.
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 3);
    }

    #[test]
    fn find_references_uses_compiler_output_when_available() {
        let json_str = r#"{
            "version": 2,
            "diagnostics": [],
            "success": true,
            "symbols": [
                {
                    "id": 1,
                    "name": "add",
                    "kind": "function",
                    "file": "/tmp/test.affine",
                    "start_line": 1,
                    "start_col": 4,
                    "end_line": 1,
                    "end_col": 7
                }
            ],
            "references": {
                "1": [
                    {"file": "/tmp/test.affine", "start_line": 3, "start_col": 10, "end_line": 3, "end_col": 13},
                    {"file": "/tmp/test.affine", "start_line": 5, "start_col": 1, "end_line": 5, "end_col": 4}
                ]
            }
        }"#;
        let json: serde_json::Value = serde_json::from_str(json_str).unwrap();
        let co = crate::symbols::CompilerOutput::from_json(&json);

        let src = "fn add(a: Int, b: Int) -> Int { a + b }";
        let uri = test_uri();
        let pos = Position { line: 0, character: 4 };

        // With declaration included.
        let refs = find_references(&uri, pos, src, true, &co);
        assert_eq!(refs.len(), 3, "2 refs + 1 declaration = 3");

        // Without declaration.
        let refs = find_references(&uri, pos, src, false, &co);
        assert_eq!(refs.len(), 2, "2 refs only");
    }
}
