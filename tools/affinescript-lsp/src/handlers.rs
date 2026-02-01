// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Request Handlers
//!
//! Implementation of LSP request handlers.

use tower_lsp::lsp_types::*;

/// Handle hover request
pub fn hover(uri: &Url, position: Position, text: &str) -> Option<Hover> {
    // Get the word at position
    let word = get_word_at_position(text, position)?;

    // For now, provide basic hover information
    // TODO: Integrate with type checker for accurate type information
    let hover_text = match word.as_str() {
        // Keywords
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
                character: position.character + word.len() as u32,
            },
        }),
    })
}

/// Handle goto definition
pub fn goto_definition(_uri: &Url, _position: Position, _text: &str) -> Option<Location> {
    // TODO: Phase 8 implementation
    // - [ ] Parse document to AST
    // - [ ] Find symbol at position
    // - [ ] Look up definition in symbol table
    // - [ ] Return location

    None
}

/// Handle find references
pub fn find_references(
    _uri: &Url,
    _position: Position,
    _text: &str,
    _include_declaration: bool,
) -> Vec<Location> {
    // TODO: Phase 8 implementation
    // - [ ] Find symbol at position
    // - [ ] Search for all references in workspace
    // - [ ] Optionally include declaration

    vec![]
}

/// Handle completion
pub fn completion(_uri: &Url, position: Position, text: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Get current line to determine context
    let line = match get_line_at_position(text, position.line) {
        Some(l) => l,
        None => return items,
    };

    let col = position.character as usize;
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

/// Handle rename
pub fn prepare_rename(_uri: &Url, _position: Position, _text: &str) -> Option<PrepareRenameResponse> {
    // TODO: Phase 8 implementation
    // - [ ] Check if position is on renameable symbol
    // - [ ] Return symbol range

    None
}

/// Handle rename
pub fn rename(
    _uri: &Url,
    _position: Position,
    _new_name: &str,
    _text: &str,
) -> Option<WorkspaceEdit> {
    // TODO: Phase 8 implementation
    // - [ ] Find all references
    // - [ ] Generate text edits for each
    // - [ ] Handle cross-file renames

    None
}

/// Handle document formatting
pub fn format(uri: &Url, text: &str, options: &FormattingOptions) -> Vec<TextEdit> {
    // Basic indentation-based formatting
    let lines: Vec<&str> = text.lines().collect();
    let mut formatted_lines = Vec::new();
    let mut indent_level = 0;
    let indent_str = if options.insert_spaces {
        " ".repeat(options.tab_size as usize)
    } else {
        "\t".to_string()
    };

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
                line: lines.len() as u32,
                character: 0,
            },
        },
        new_text: formatted_text,
    }]
}

/// Handle code actions
pub fn code_actions(_uri: &Url, _range: Range, _diagnostics: &[Diagnostic]) -> Vec<CodeAction> {
    // TODO: Phase 8 implementation
    // - [ ] Generate quick fixes for diagnostics
    // - [ ] Add refactoring actions
    //   - [ ] Extract function
    //   - [ ] Extract variable
    //   - [ ] Inline variable
    //   - [ ] Add type annotation
    //   - [ ] Convert to/from effect style

    vec![]
}

/// Handle document symbols
pub fn document_symbols(_uri: &Url, text: &str) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    let lines: Vec<&str> = text.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        let trimmed = line.trim();

        // Match function definitions
        if trimmed.starts_with("fn ") {
            if let Some(name_start) = trimmed.find("fn ").map(|i| i + 3) {
                if let Some(name_end) = trimmed[name_start..].find(|c: char| c == '(' || c.is_whitespace()) {
                    let name = &trimmed[name_start..name_start + name_end];
                    symbols.push(DocumentSymbol {
                        name: name.to_string(),
                        detail: Some(trimmed.to_string()),
                        kind: SymbolKind::FUNCTION,
                        range: Range {
                            start: Position { line: line_num as u32, character: 0 },
                            end: Position { line: line_num as u32, character: line.len() as u32 },
                        },
                        selection_range: Range {
                            start: Position { line: line_num as u32, character: name_start as u32 },
                            end: Position { line: line_num as u32, character: (name_start + name.len()) as u32 },
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
            let keyword_len = if trimmed.starts_with("type ") { 5 } else { if trimmed.starts_with("struct ") { 7 } else { 5 } };
            if let Some(name_end) = trimmed[keyword_len..].find(|c: char| c == '=' || c == '{' || c.is_whitespace()) {
                let name = &trimmed[keyword_len..keyword_len + name_end].trim();
                symbols.push(DocumentSymbol {
                    name: name.to_string(),
                    detail: Some(trimmed.to_string()),
                    kind: SymbolKind::STRUCT,
                    range: Range {
                        start: Position { line: line_num as u32, character: 0 },
                        end: Position { line: line_num as u32, character: line.len() as u32 },
                    },
                    selection_range: Range {
                        start: Position { line: line_num as u32, character: keyword_len as u32 },
                        end: Position { line: line_num as u32, character: (keyword_len + name.len()) as u32 },
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
    // TODO: Phase 8 implementation
    // - [ ] Determine if inside function call
    // - [ ] Get function signature
    // - [ ] Highlight active parameter

    None
}

/// Handle inlay hints
pub fn inlay_hints(_uri: &Url, _range: Range, _text: &str) -> Vec<InlayHint> {
    // TODO: Phase 8 implementation
    // - [ ] Find bindings without explicit types
    // - [ ] Add type hints
    // - [ ] Add parameter name hints at call sites
    // - [ ] Add lifetime hints (if not obvious)

    vec![]
}

/// Get word at position in text
fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;

    if col >= line.len() {
        return None;
    }

    // Find word boundaries
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

// TODO: Phase 8 enhancements
// - [ ] Connect to AffineScript compiler for full semantic analysis
// - [ ] Implement caching for performance
// - [ ] Add semantic tokens for syntax highlighting
// - [ ] Add call hierarchy
// - [ ] Add type hierarchy
