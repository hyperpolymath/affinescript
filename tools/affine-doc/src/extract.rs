// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Documentation Extraction
//!
//! Extracts documentation from AffineScript source code.

use std::path::Path;

/// Extracted documentation for a module
#[derive(Debug, Clone)]
pub struct ModuleDoc {
    /// Module path (e.g., "std::collections::vec")
    pub path: Vec<String>,

    /// Module-level documentation
    pub doc: Option<String>,

    /// Items in this module
    pub items: Vec<ItemDoc>,

    /// Submodules
    pub submodules: Vec<String>,
}

/// Documentation for an item
#[derive(Debug, Clone)]
pub struct ItemDoc {
    /// Item name
    pub name: String,

    /// Item kind
    pub kind: ItemKind,

    /// Documentation comment
    pub doc: Option<String>,

    /// Type signature
    pub signature: String,

    /// Source location
    pub location: SourceLocation,

    /// Visibility
    pub visibility: Visibility,

    /// Type parameters
    pub type_params: Vec<TypeParamDoc>,

    /// Effects (for functions)
    pub effects: Vec<String>,

    /// Examples from doc comments
    pub examples: Vec<String>,
}

/// Item kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemKind {
    /// Function
    Function,
    /// Type alias
    TypeAlias,
    /// Struct
    Struct,
    /// Enum
    Enum,
    /// Trait
    Trait,
    /// Effect
    Effect,
    /// Constant
    Const,
    /// Static
    Static,
    /// Impl block
    Impl,
    /// Module
    Module,
}

impl ItemKind {
    /// Display name for the item kind
    pub fn display_name(&self) -> &'static str {
        match self {
            ItemKind::Function => "Function",
            ItemKind::TypeAlias => "Type Alias",
            ItemKind::Struct => "Struct",
            ItemKind::Enum => "Enum",
            ItemKind::Trait => "Trait",
            ItemKind::Effect => "Effect",
            ItemKind::Const => "Constant",
            ItemKind::Static => "Static",
            ItemKind::Impl => "Implementation",
            ItemKind::Module => "Module",
        }
    }
}

/// Visibility level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    /// Public
    Public,
    /// Public within crate
    Crate,
    /// Private
    Private,
}

/// Source location
#[derive(Debug, Clone)]
pub struct SourceLocation {
    /// File path
    pub file: String,
    /// Line number
    pub line: u32,
    /// Column number
    pub column: u32,
}

/// Type parameter documentation
#[derive(Debug, Clone)]
pub struct TypeParamDoc {
    /// Parameter name
    pub name: String,
    /// Bounds
    pub bounds: Vec<String>,
    /// Default value
    pub default: Option<String>,
}

/// Documentation extractor
pub struct Extractor {
    /// Include private items
    include_private: bool,
}

impl Extractor {
    /// Create a new extractor
    pub fn new(include_private: bool) -> Self {
        Extractor { include_private }
    }

    /// Extract documentation from a source file
    pub fn extract_file(&self, path: impl AsRef<Path>) -> anyhow::Result<ModuleDoc> {
        let _path = path.as_ref();

        // TODO: Phase 8 implementation
        // - [ ] Parse source file
        // - [ ] Walk AST
        // - [ ] Extract doc comments
        // - [ ] Build ModuleDoc

        Ok(ModuleDoc {
            path: vec![],
            doc: None,
            items: vec![],
            submodules: vec![],
        })
    }

    /// Extract documentation from a directory
    pub fn extract_dir(&self, path: impl AsRef<Path>) -> anyhow::Result<Vec<ModuleDoc>> {
        let _path = path.as_ref();

        // TODO: Phase 8 implementation
        // - [ ] Find all .afs files
        // - [ ] Extract each file
        // - [ ] Build module hierarchy

        Ok(vec![])
    }

    /// Parse doc comment into structured sections
    pub fn parse_doc_comment(&self, comment: &str) -> DocComment {
        let mut description = String::new();
        let mut params = vec![];
        let mut returns = None;
        let mut examples = vec![];
        let mut panics = None;
        let mut safety = None;

        let mut current_section = "description";
        let mut current_content = String::new();

        for line in comment.lines() {
            let line = line.trim();

            // Check for section headers
            if line.starts_with("# ") {
                // Save previous section
                self.save_section(
                    current_section,
                    &current_content,
                    &mut description,
                    &mut params,
                    &mut returns,
                    &mut examples,
                    &mut panics,
                    &mut safety,
                );

                // Start new section
                current_section = match line.to_lowercase().as_str() {
                    "# parameters" | "# arguments" => "params",
                    "# returns" => "returns",
                    "# examples" | "# example" => "examples",
                    "# panics" => "panics",
                    "# safety" => "safety",
                    _ => "description",
                };
                current_content.clear();
            } else {
                current_content.push_str(line);
                current_content.push('\n');
            }
        }

        // Save last section
        self.save_section(
            current_section,
            &current_content,
            &mut description,
            &mut params,
            &mut returns,
            &mut examples,
            &mut panics,
            &mut safety,
        );

        DocComment {
            description,
            params,
            returns,
            examples,
            panics,
            safety,
        }
    }

    fn save_section(
        &self,
        section: &str,
        content: &str,
        description: &mut String,
        _params: &mut Vec<(String, String)>,
        returns: &mut Option<String>,
        examples: &mut Vec<String>,
        panics: &mut Option<String>,
        safety: &mut Option<String>,
    ) {
        let content = content.trim();
        if content.is_empty() {
            return;
        }

        match section {
            "description" => *description = content.to_string(),
            "params" => {
                // TODO: Parse parameter docs
            }
            "returns" => *returns = Some(content.to_string()),
            "examples" => examples.push(content.to_string()),
            "panics" => *panics = Some(content.to_string()),
            "safety" => *safety = Some(content.to_string()),
            _ => {}
        }
    }
}

/// Parsed doc comment
#[derive(Debug, Clone)]
pub struct DocComment {
    /// Main description
    pub description: String,
    /// Parameter documentation
    pub params: Vec<(String, String)>,
    /// Return value documentation
    pub returns: Option<String>,
    /// Example code blocks
    pub examples: Vec<String>,
    /// Panic conditions
    pub panics: Option<String>,
    /// Safety requirements (for unsafe functions)
    pub safety: Option<String>,
}

// TODO: Phase 8 implementation
// - [ ] Connect to AffineScript parser
// - [ ] Handle attribute macros (#[doc], #[deprecated], etc.)
// - [ ] Extract impl blocks
// - [ ] Handle re-exports
// - [ ] Support module-level docs (//! comments)
