// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Diagnostics
//!
//! Converts AffineScript compiler diagnostics to LSP diagnostics.

use tower_lsp::lsp_types::*;

/// Diagnostic severity mapping
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl From<Severity> for DiagnosticSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Info => DiagnosticSeverity::INFORMATION,
            Severity::Hint => DiagnosticSeverity::HINT,
        }
    }
}

/// AffineScript diagnostic category
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticCategory {
    /// Parse error
    Parse,
    /// Type error
    Type,
    /// Borrow checker error
    Borrow,
    /// Effect error
    Effect,
    /// Quantity error
    Quantity,
    /// Name resolution error
    Name,
    /// Refinement type error
    Refinement,
    /// Warning
    Warning,
    /// Lint
    Lint,
}

impl DiagnosticCategory {
    /// Get the error code prefix
    pub fn code_prefix(&self) -> &'static str {
        match self {
            DiagnosticCategory::Parse => "E0",
            DiagnosticCategory::Type => "E1",
            DiagnosticCategory::Borrow => "E2",
            DiagnosticCategory::Effect => "E3",
            DiagnosticCategory::Quantity => "E4",
            DiagnosticCategory::Name => "E5",
            DiagnosticCategory::Refinement => "E6",
            DiagnosticCategory::Warning => "W",
            DiagnosticCategory::Lint => "L",
        }
    }
}

/// AffineScript diagnostic
#[derive(Debug, Clone)]
pub struct AfsDiagnostic {
    /// Error category
    pub category: DiagnosticCategory,
    /// Error code (within category)
    pub code: u32,
    /// Error message
    pub message: String,
    /// Primary span
    pub span: AfsSpan,
    /// Additional labeled spans
    pub labels: Vec<(AfsSpan, String)>,
    /// Help text
    pub help: Option<String>,
    /// Note text
    pub note: Option<String>,
}

/// Source span
#[derive(Debug, Clone)]
pub struct AfsSpan {
    pub file: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

impl AfsSpan {
    /// Convert to LSP range
    pub fn to_range(&self) -> Range {
        Range {
            start: Position {
                line: self.start_line.saturating_sub(1),
                character: self.start_col.saturating_sub(1),
            },
            end: Position {
                line: self.end_line.saturating_sub(1),
                character: self.end_col.saturating_sub(1),
            },
        }
    }
}

impl AfsDiagnostic {
    /// Convert to LSP diagnostic
    pub fn to_lsp(&self) -> Diagnostic {
        let severity = match self.category {
            DiagnosticCategory::Warning | DiagnosticCategory::Lint => Severity::Warning,
            _ => Severity::Error,
        };

        let mut related = Vec::new();
        for (span, label) in &self.labels {
            related.push(DiagnosticRelatedInformation {
                location: Location {
                    uri: Url::parse(&format!("file://{}", span.file)).unwrap(),
                    range: span.to_range(),
                },
                message: label.clone(),
            });
        }

        let mut message = self.message.clone();
        if let Some(help) = &self.help {
            message.push_str(&format!("\n\nhelp: {}", help));
        }
        if let Some(note) = &self.note {
            message.push_str(&format!("\n\nnote: {}", note));
        }

        Diagnostic {
            range: self.span.to_range(),
            severity: Some(severity.into()),
            code: Some(NumberOrString::String(format!(
                "{}{}",
                self.category.code_prefix(),
                self.code
            ))),
            code_description: Some(CodeDescription {
                href: Url::parse(&format!(
                    "https://affinescript.dev/errors/{}{}",
                    self.category.code_prefix(),
                    self.code
                ))
                .unwrap(),
            }),
            source: Some("affinescript".to_string()),
            message,
            related_information: if related.is_empty() {
                None
            } else {
                Some(related)
            },
            tags: None,
            data: None,
        }
    }
}

/// Convert compiler diagnostics to LSP diagnostics
pub fn convert_diagnostics(_diagnostics: Vec<AfsDiagnostic>) -> Vec<Diagnostic> {
    // TODO: Phase 8 implementation
    // - [ ] Connect to compiler diagnostic output
    // - [ ] Convert spans correctly
    // - [ ] Handle multi-file diagnostics

    vec![]
}

// TODO: Phase 8 implementation
// - [ ] Import actual diagnostic types from compiler
// - [ ] Implement diagnostic code links
// - [ ] Add diagnostic tags (deprecated, unnecessary)
// - [ ] Support diagnostic quickfixes
