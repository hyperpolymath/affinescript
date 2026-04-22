#![forbid(unsafe_code)]
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! AffineScript Language Server
//!
//! Provides IDE features via the Language Server Protocol:
//! - Diagnostics (errors, warnings) — via `affinescript check --json`
//! - Hover information
//! - Go to definition (Phase B)
//! - Find references (Phase C)
//! - Code completion
//! - Rename (Phase C)
//! - Formatting
//! - Code actions (Phase D)
//!
//! ## Phase A: --json contract
//!
//! The LSP invokes the compiler with `--json` and receives a single JSON
//! object on stderr matching this schema:
//!
//! ```json
//! {
//!   "version": 1,
//!   "diagnostics": [ { "severity", "code", "message", "file",
//!                       "start_line", "start_col", "end_line", "end_col",
//!                       "help", "labels" } ],
//!   "success": true | false
//! }
//! ```
//!
//! This replaces the fragile regex parsing from the pre-Phase-A implementation
//! and is the foundation for Phases B-D.

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod capabilities;
mod diagnostics;
mod document;
mod handlers;
pub mod symbols;
pub mod text_index;

/// The AffineScript language server backend
#[derive(Debug)]
struct Backend {
    /// LSP client for sending notifications
    client: Client,
    /// Document manager
    documents: document::DocumentManager,
    /// Phase B: cached compiler symbol output (updated on each check).
    compiler_output: std::sync::Mutex<symbols::CompilerOutput>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Backend {
            client,
            documents: document::DocumentManager::new(),
            compiler_output: std::sync::Mutex::new(symbols::CompilerOutput::default()),
        }
    }

    /// Check a document by calling `affinescript check --json`.
    ///
    /// Parses the structured JSON output instead of regex-matching stderr
    /// text.  Falls back to an internal error diagnostic if the compiler
    /// is not found or returns unparseable output.
    async fn check_document(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        use tokio::process::Command;
        use std::process::Stdio;

        // Write source to a temp file so the compiler can read it
        let temp_path = std::env::temp_dir().join(format!("lsp_{}.affine", uuid::Uuid::new_v4()));
        if let Err(e) = tokio::fs::write(&temp_path, text).await {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to write temp file: {}", e))
                .await;
            return vec![];
        }

        // Run `affinescript check --json <path>`
        let output = match Command::new("affinescript")
            .arg("check")
            .arg("--json")
            .arg(&temp_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
        {
            Ok(o) => o,
            Err(e) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to run affinescript: {}", e),
                    )
                    .await;
                let _ = tokio::fs::remove_file(&temp_path).await;
                return vec![];
            }
        };

        // Clean up temp file
        let _ = tokio::fs::remove_file(&temp_path).await;

        // Parse the JSON diagnostic report from stderr
        let stderr = String::from_utf8_lossy(&output.stderr);
        self.parse_json_diagnostics(&stderr, uri)
    }

    /// Parse the compiler's `--json` diagnostic report into LSP diagnostics.
    ///
    /// Expected shape (version 1):
    /// ```json
    /// {
    ///   "version": 1,
    ///   "diagnostics": [ ... ],
    ///   "success": bool
    /// }
    /// ```
    fn parse_json_diagnostics(&self, stderr: &str, _uri: &Url) -> Vec<Diagnostic> {
        let json: serde_json::Value = match serde_json::from_str(stderr.trim()) {
            Ok(v) => v,
            Err(_) => {
                // If the compiler didn't produce valid JSON (e.g. crash, old
                // binary without --json), fall back to the legacy regex parser
                // so the LSP degrades gracefully.
                return self.parse_diagnostics_legacy(stderr, _uri);
            }
        };

        let version = json.get("version").and_then(|v| v.as_i64()).unwrap_or(0);
        if version < 1 || version > 2 {
            // Unknown protocol version — fall back to legacy
            return self.parse_diagnostics_legacy(stderr, _uri);
        }

        // Phase B: v2 includes symbols and references for goto-def.
        if version >= 2 {
            let output = symbols::CompilerOutput::from_json(&json);
            if let Ok(mut cached) = self.compiler_output.lock() {
                *cached = output;
            }
        }

        let diags_array = match json.get("diagnostics").and_then(|d| d.as_array()) {
            Some(a) => a,
            None => return vec![],
        };

        let mut diagnostics = Vec::with_capacity(diags_array.len());

        for entry in diags_array {
            let severity_str = entry.get("severity").and_then(|s| s.as_str()).unwrap_or("error");
            let code = entry.get("code").and_then(|c| c.as_str()).unwrap_or("E0000");
            let message = entry.get("message").and_then(|m| m.as_str()).unwrap_or("");
            let start_line = entry.get("start_line").and_then(|l| l.as_u64()).unwrap_or(1) as u32;
            let start_col = entry.get("start_col").and_then(|c| c.as_u64()).unwrap_or(1) as u32;
            let end_line = entry.get("end_line").and_then(|l| l.as_u64()).unwrap_or(start_line as u64) as u32;
            let end_col = entry.get("end_col").and_then(|c| c.as_u64()).unwrap_or(start_col as u64 + 1) as u32;
            let help = entry.get("help").and_then(|h| h.as_str());

            let severity = match severity_str {
                "error" => DiagnosticSeverity::ERROR,
                "warning" => DiagnosticSeverity::WARNING,
                "hint" => DiagnosticSeverity::HINT,
                "info" | "note" => DiagnosticSeverity::INFORMATION,
                _ => DiagnosticSeverity::WARNING,
            };

            // Build related information from labels array
            let mut related = Vec::new();
            if let Some(labels) = entry.get("labels").and_then(|l| l.as_array()) {
                for label in labels {
                    let lbl_file = label.get("file").and_then(|f| f.as_str()).unwrap_or("");
                    let lbl_start_line = label.get("start_line").and_then(|l| l.as_u64()).unwrap_or(1) as u32;
                    let lbl_start_col = label.get("start_col").and_then(|c| c.as_u64()).unwrap_or(1) as u32;
                    let lbl_end_line = label.get("end_line").and_then(|l| l.as_u64()).unwrap_or(lbl_start_line as u64) as u32;
                    let lbl_end_col = label.get("end_col").and_then(|c| c.as_u64()).unwrap_or(lbl_start_col as u64 + 1) as u32;
                    let lbl_msg = label.get("message").and_then(|m| m.as_str()).unwrap_or("");

                    if let Ok(lbl_uri) = Url::from_file_path(lbl_file) {
                        related.push(DiagnosticRelatedInformation {
                            location: Location {
                                uri: lbl_uri,
                                range: Range {
                                    start: Position {
                                        line: lbl_start_line.saturating_sub(1),
                                        character: lbl_start_col.saturating_sub(1),
                                    },
                                    end: Position {
                                        line: lbl_end_line.saturating_sub(1),
                                        character: lbl_end_col.saturating_sub(1),
                                    },
                                },
                            },
                            message: lbl_msg.to_string(),
                        });
                    }
                }
            }

            // Append help text to message if present
            let full_message = match help {
                Some(h) => format!("{}\n\nhelp: {}", message, h),
                None => message.to_string(),
            };

            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: start_line.saturating_sub(1),
                        character: start_col.saturating_sub(1),
                    },
                    end: Position {
                        line: end_line.saturating_sub(1),
                        character: end_col.saturating_sub(1),
                    },
                },
                severity: Some(severity),
                code: Some(NumberOrString::String(code.to_string())),
                source: Some("affinescript".to_string()),
                message: full_message,
                related_information: if related.is_empty() { None } else { Some(related) },
                tags: None,
                code_description: None,
                data: None,
            });
        }

        diagnostics
    }

    /// Legacy regex-based parser — kept as a fallback for when the compiler
    /// binary predates `--json` support.
    fn parse_diagnostics_legacy(&self, output: &str, uri: &Url) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for line in output.lines() {
            if let Some(diagnostic) = self.parse_diagnostic_line(line, uri) {
                diagnostics.push(diagnostic);
            }
        }

        diagnostics
    }

    /// Parse a single diagnostic line (legacy format)
    fn parse_diagnostic_line(&self, line: &str, _uri: &Url) -> Option<Diagnostic> {
        use regex::Regex;

        let re = Regex::new(
            r"(.+):(\d+):(\d+):\s*(error|warning|hint|info|note)\s*(?:\[([A-Z]\d+)\])?:\s*(.+)"
        ).ok()?;
        let caps = re.captures(line)?;

        let line = caps.get(2)?.as_str().parse::<u32>().ok()?.saturating_sub(1);
        let col = caps.get(3)?.as_str().parse::<u32>().ok()?.saturating_sub(1);
        let severity_str = caps.get(4)?.as_str();
        let error_code = caps.get(5).map(|m| m.as_str().to_string());
        let message = caps.get(6)?.as_str();

        let severity = match severity_str {
            "error" => DiagnosticSeverity::ERROR,
            "warning" => DiagnosticSeverity::WARNING,
            "hint" => DiagnosticSeverity::HINT,
            "info" | "note" => DiagnosticSeverity::INFORMATION,
            _ => DiagnosticSeverity::WARNING,
        };

        Some(Diagnostic {
            range: Range {
                start: Position {
                    line,
                    character: col,
                },
                end: Position {
                    line,
                    character: col + 1,
                },
            },
            severity: Some(severity),
            code: error_code.map(NumberOrString::String),
            source: Some("affinescript".to_string()),
            message: message.to_string(),
            related_information: None,
            tags: None,
            code_description: None,
            data: None,
        })
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: capabilities::server_capabilities(),
            server_info: Some(ServerInfo {
                name: "affinescript-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "AffineScript LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "Document opened")
            .await;

        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        // Store the document so handlers can access its text.
        self.documents.open(uri.clone(), text.clone(), version);

        // Run type checker via --json
        let diagnostics = self.check_document(&uri, &text).await;

        // Publish diagnostics
        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Apply incremental edits to the stored document.
        self.documents.apply_changes(&uri, version, params.content_changes.clone());

        // Get the latest text from the document manager.
        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => {
                // Fallback: use the last change's full text if available.
                match params.content_changes.last() {
                    Some(change) => change.text.clone(),
                    None => return,
                }
            }
        };

        // Run type checker via --json
        let diagnostics = self.check_document(&uri, &text).await;

        // Publish diagnostics
        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        // Remove document from manager
        self.documents.close(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let co = self.compiler_output.lock().unwrap();
        Ok(handlers::hover(&uri, position, &text, &co))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let co = self.compiler_output.lock().unwrap();
        match handlers::goto_definition(&uri, position, &text, &co) {
            Some(loc) => Ok(Some(GotoDefinitionResponse::Scalar(loc))),
            None => Ok(None),
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let co = self.compiler_output.lock().unwrap();
        let refs = handlers::find_references(&uri, position, &text, include_declaration, &co);
        if refs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(refs))
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let items = handlers::completion(&uri, position, &text);
        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let co = self.compiler_output.lock().unwrap();
        Ok(handlers::prepare_rename(&uri, position, &text, &co))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let co = self.compiler_output.lock().unwrap();
        Ok(handlers::rename(&uri, position, &new_name, &text, &co))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;

        let text = match self.documents.get_text(&uri) {
            Some(t) => t,
            None => return Ok(None),
        };

        let edits = handlers::format(&uri, &text, &params.options);
        if edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(edits))
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        // TODO: Phase D — requires structured fix suggestions in --json output
        let _ = params;
        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    tracing::info!("Starting AffineScript Language Server (Phase A: --json diagnostics)");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

// Phase B: Symbol table in --json output → go-to-definition, hover with types
// Phase C: Reference index in --json output → find-references, rename
// Phase D: Fix suggestions in --json output → code actions, inlay hints
