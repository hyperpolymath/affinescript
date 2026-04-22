// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! Text-Based Symbol Index (Phase C)
//!
//! Provides a fallback symbol index by scanning AffineScript source text for
//! definitions and usages when the compiler's v2 JSON output is unavailable.
//!
//! This module recognises:
//! - `fn <name>` — function definitions
//! - `let <name>` — variable bindings
//! - `type <name>` — type aliases
//! - `struct <name>` — structure definitions
//! - `enum <name>` — enumeration definitions
//! - `effect <name>` — effect type definitions
//! - `handler <name>` — effect handler definitions
//!
//! For each definition it records the name, location, and kind.  It then
//! scans the entire document for all occurrences of that name as an identifier
//! (i.e. bounded by non-alphanumeric/non-underscore characters) to build a
//! reference list.
//!
//! This is intentionally simple — it does not parse the AST or resolve scopes.
//! When the compiler provides v2 JSON with full symbol tables, those take
//! priority.

/// A definition found by text scanning.
#[derive(Debug, Clone)]
pub struct TextDefinition {
    /// The symbol name (e.g. "foo", "MyType").
    pub name: String,
    /// The kind of definition (e.g. "function", "variable", "type").
    pub kind: &'static str,
    /// Zero-based line number where the name starts.
    pub line: u32,
    /// Zero-based UTF-16 column where the name starts.
    pub col_start: u32,
    /// Zero-based UTF-16 column where the name ends (exclusive).
    pub col_end: u32,
}

/// A usage (reference) found by text scanning.
#[derive(Debug, Clone)]
pub struct TextReference {
    /// The symbol name.
    pub name: String,
    /// Zero-based line number.
    pub line: u32,
    /// Zero-based UTF-16 column where the name starts.
    pub col_start: u32,
    /// Zero-based UTF-16 column where the name ends (exclusive).
    pub col_end: u32,
}

/// A complete text-based index for a single document.
#[derive(Debug, Clone, Default)]
pub struct TextIndex {
    /// All definitions found in the document.
    pub definitions: Vec<TextDefinition>,
    /// All identifier occurrences found in the document.
    pub occurrences: Vec<TextReference>,
}

/// AffineScript keywords that should never be treated as user-defined symbols.
/// Renaming these would break the language syntax.
const KEYWORDS: &[&str] = &[
    "fn", "let", "type", "struct", "enum", "effect", "handler",
    "match", "if", "else", "for", "while", "return", "true", "false",
    "linear", "affine", "unrestricted", "borrow", "move", "mut",
    "pub", "mod", "use", "import", "as", "in", "do", "with",
    "where", "trait", "impl", "self", "Self",
];

/// Standard library type names that should not be renamed.
const BUILTIN_TYPES: &[&str] = &[
    "Int", "Float", "Bool", "String", "Unit", "List", "Option", "Result",
];

impl TextIndex {
    /// Build a text-based index from source text.
    ///
    /// Scans every line for definition keywords (`fn`, `let`, `type`, etc.)
    /// and records the name that follows.  Then scans every line for all
    /// identifier occurrences to populate the occurrence list.
    pub fn build(text: &str) -> Self {
        let mut index = TextIndex::default();
        let lines: Vec<&str> = text.lines().collect();

        // Pass 1: find definitions.
        for (line_num, line) in lines.iter().enumerate() {
            let trimmed = line.trim_start();
            let leading_bytes = line.len() - trimmed.len();

            // Each entry: (keyword, kind label)
            let def_keywords: &[(&str, &str)] = &[
                ("fn ", "function"),
                ("let ", "variable"),
                ("type ", "type"),
                ("struct ", "struct"),
                ("enum ", "enum"),
                ("effect ", "effect"),
                ("handler ", "handler"),
            ];

            for &(keyword, kind) in def_keywords {
                if !trimmed.starts_with(keyword) {
                    continue;
                }
                let after_keyword = &trimmed[keyword.len()..];
                // Extract the identifier that follows the keyword.
                let name_end = after_keyword
                    .find(|c: char| !c.is_alphanumeric() && c != '_')
                    .unwrap_or(after_keyword.len());
                if name_end == 0 {
                    continue;
                }
                let name = &after_keyword[..name_end];
                if name.is_empty() {
                    continue;
                }

                // Compute UTF-16 columns.
                let byte_offset_of_name = leading_bytes + keyword.len();
                let col_start = utf16_len(&line[..byte_offset_of_name]);
                let col_end = col_start + utf16_len(name);

                index.definitions.push(TextDefinition {
                    name: name.to_string(),
                    kind,
                    line: line_num as u32,
                    col_start,
                    col_end,
                });
                break; // Only one definition per line.
            }
        }

        // Pass 2: find all identifier occurrences across the document.
        // We scan for every identifier token and record it.
        for (line_num, line) in lines.iter().enumerate() {
            let mut byte_pos = 0;
            let chars: Vec<char> = line.chars().collect();
            let mut char_idx = 0;

            while char_idx < chars.len() {
                let c = chars[char_idx];

                // Skip comment lines (// style).
                if c == '/' && char_idx + 1 < chars.len() && chars[char_idx + 1] == '/' {
                    break; // Rest of line is a comment.
                }

                // Skip string literals.
                if c == '"' {
                    byte_pos += c.len_utf8();
                    char_idx += 1;
                    while char_idx < chars.len() && chars[char_idx] != '"' {
                        if chars[char_idx] == '\\' && char_idx + 1 < chars.len() {
                            byte_pos += chars[char_idx].len_utf8();
                            char_idx += 1;
                        }
                        byte_pos += chars[char_idx].len_utf8();
                        char_idx += 1;
                    }
                    if char_idx < chars.len() {
                        byte_pos += chars[char_idx].len_utf8();
                        char_idx += 1;
                    }
                    continue;
                }

                if c.is_alphanumeric() || c == '_' {
                    // Start of an identifier token.
                    let ident_start_byte = byte_pos;
                    let mut ident_end_byte = byte_pos + c.len_utf8();
                    let start_char_idx = char_idx;
                    char_idx += 1;

                    while char_idx < chars.len()
                        && (chars[char_idx].is_alphanumeric() || chars[char_idx] == '_')
                    {
                        ident_end_byte += chars[char_idx].len_utf8();
                        char_idx += 1;
                    }

                    let ident = &line[ident_start_byte..ident_end_byte];

                    // Skip identifiers that start with a digit (they are numbers).
                    if !chars[start_char_idx].is_ascii_digit() {
                        let col_start = utf16_len(&line[..ident_start_byte]);
                        let col_end = col_start + utf16_len(ident);

                        index.occurrences.push(TextReference {
                            name: ident.to_string(),
                            line: line_num as u32,
                            col_start,
                            col_end,
                        });
                    }

                    byte_pos = ident_end_byte;
                } else {
                    byte_pos += c.len_utf8();
                    char_idx += 1;
                }
            }
        }

        index
    }

    /// Find the definition of a symbol by name.
    ///
    /// Returns the first definition matching the given name.
    pub fn find_definition(&self, name: &str) -> Option<&TextDefinition> {
        self.definitions.iter().find(|d| d.name == name)
    }

    /// Find all occurrences of a symbol by name.
    ///
    /// Returns every occurrence (both definition sites and usage sites).
    pub fn find_occurrences(&self, name: &str) -> Vec<&TextReference> {
        self.occurrences.iter().filter(|o| o.name == name).collect()
    }

    /// Find all occurrences of a symbol excluding the definition site.
    ///
    /// Useful for find-references when `include_declaration` is false.
    pub fn find_usages_only(&self, name: &str) -> Vec<&TextReference> {
        let def = self.find_definition(name);
        self.occurrences
            .iter()
            .filter(|o| {
                o.name == name
                    && !def.map_or(false, |d| {
                        o.line == d.line && o.col_start == d.col_start
                    })
            })
            .collect()
    }

    /// Find the identifier at a given cursor position.
    ///
    /// Returns the occurrence whose range contains the position.
    pub fn find_at_position(&self, line: u32, character: u32) -> Option<&TextReference> {
        self.occurrences.iter().find(|o| {
            o.line == line && character >= o.col_start && character < o.col_end
        })
    }
}

/// Check whether a name is a keyword or builtin and therefore not renameable.
pub fn is_renameable(name: &str) -> bool {
    !KEYWORDS.contains(&name) && !BUILTIN_TYPES.contains(&name)
}

/// Validate that a proposed new name is a legal AffineScript identifier.
///
/// Must start with a letter or underscore, contain only alphanumerics and
/// underscores, and must not be a keyword or builtin type name.
pub fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let mut chars = name.chars();
    let first = chars.next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }
    if !chars.all(|c| c.is_alphanumeric() || c == '_') {
        return false;
    }
    // Must not rename to a keyword or builtin.
    is_renameable(name)
}

/// Count UTF-16 code units in a byte slice of a string.
///
/// This mirrors the helper in handlers.rs but is kept local to avoid
/// circular module dependencies.
fn utf16_len(s: &str) -> u32 {
    s.chars().map(|c| c.len_utf16()).sum::<usize>() as u32
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: build index from source and return it.
    fn idx(src: &str) -> TextIndex {
        TextIndex::build(src)
    }

    #[test]
    fn finds_function_definition() {
        let index = idx("fn greet(name: String) -> Unit {\n  let x = 1\n}");
        assert_eq!(index.definitions.len(), 2); // greet + x
        assert_eq!(index.definitions[0].name, "greet");
        assert_eq!(index.definitions[0].kind, "function");
        assert_eq!(index.definitions[0].line, 0);
        assert_eq!(index.definitions[1].name, "x");
        assert_eq!(index.definitions[1].kind, "variable");
        assert_eq!(index.definitions[1].line, 1);
    }

    #[test]
    fn finds_type_definitions() {
        let src = "type Foo = Int\nstruct Bar {\n  x: Int\n}\nenum Baz { A, B }";
        let index = idx(src);
        let names: Vec<&str> = index.definitions.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"Foo"));
        assert!(names.contains(&"Bar"));
        assert!(names.contains(&"Baz"));
    }

    #[test]
    fn finds_all_occurrences() {
        let src = "let x = 1\nlet y = x + 2\nfn foo() -> Int { x }";
        let index = idx(src);
        let x_refs = index.find_occurrences("x");
        // "x" appears: definition, usage in y binding, usage in foo body = 3
        assert_eq!(x_refs.len(), 3);
    }

    #[test]
    fn find_usages_only_excludes_definition() {
        let src = "let x = 1\nlet y = x + 2";
        let index = idx(src);
        let usages = index.find_usages_only("x");
        assert_eq!(usages.len(), 1);
        assert_eq!(usages[0].line, 1);
    }

    #[test]
    fn find_at_position_works() {
        let src = "fn hello() -> Unit {}";
        let index = idx(src);
        // "hello" starts at UTF-16 col 3 (after "fn "), ends at 8.
        let hit = index.find_at_position(0, 4);
        assert!(hit.is_some());
        assert_eq!(hit.unwrap().name, "hello");
    }

    #[test]
    fn keywords_are_not_renameable() {
        assert!(!is_renameable("fn"));
        assert!(!is_renameable("let"));
        assert!(!is_renameable("Int"));
        assert!(is_renameable("myVar"));
    }

    #[test]
    fn validates_identifiers() {
        assert!(is_valid_identifier("foo"));
        assert!(is_valid_identifier("_bar"));
        assert!(is_valid_identifier("Baz123"));
        assert!(!is_valid_identifier("123abc"));
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("fn")); // keyword
    }

    #[test]
    fn skips_comments() {
        let src = "let x = 1 // x is important\nlet y = x";
        let index = idx(src);
        let x_refs = index.find_occurrences("x");
        // definition on line 0, usage on line 1 = 2 (comment "x" skipped)
        assert_eq!(x_refs.len(), 2);
    }

    #[test]
    fn skips_string_literals() {
        let src = "let x = \"x is a string\"\nlet y = x";
        let index = idx(src);
        let x_refs = index.find_occurrences("x");
        // definition on line 0, usage on line 1 = 2 ("x" in string skipped)
        assert_eq!(x_refs.len(), 2);
    }
}
