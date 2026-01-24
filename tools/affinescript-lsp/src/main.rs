// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! AffineScript Language Server
//!
//! Provides IDE features via the Language Server Protocol:
//! - Diagnostics (errors, warnings)
//! - Hover information
//! - Go to definition
//! - Find references
//! - Code completion
//! - Rename
//! - Formatting
//! - Code actions

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod capabilities;
mod diagnostics;
mod document;
mod handlers;

/// The AffineScript language server backend
#[derive(Debug)]
struct Backend {
    /// LSP client for sending notifications
    client: Client,
    /// Document manager
    documents: document::DocumentManager,
}

impl Backend {
    fn new(client: Client) -> Self {
        Backend {
            client,
            documents: document::DocumentManager::new(),
        }
    }

    /// Check a document by calling the affinescript compiler
    async fn check_document(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        use std::process::Stdio;
        use tokio::io::AsyncWriteExt;
        use tokio::process::Command;

        let path = match uri.to_file_path() {
            Ok(p) => p,
            Err(_) => return vec![],
        };

        // Write temp file
        let temp_path = std::env::temp_dir().join(format!("lsp_{}.as", uuid::Uuid::new_v4()));
        if let Err(e) = tokio::fs::write(&temp_path, text).await {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to write temp file: {}", e))
                .await;
            return vec![];
        }

        // Run affinescript check
        let output = match Command::new("affinescript")
            .arg("check")
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
                return vec![];
            }
        };

        // Clean up temp file
        let _ = tokio::fs::remove_file(&temp_path).await;

        // Parse diagnostics from stderr
        let stderr = String::from_utf8_lossy(&output.stderr);
        self.parse_diagnostics(&stderr, uri)
    }

    /// Parse compiler output into LSP diagnostics
    fn parse_diagnostics(&self, output: &str, uri: &Url) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for line in output.lines() {
            // Parse format: "file:line:col: severity: message"
            if let Some(diagnostic) = self.parse_diagnostic_line(line, uri) {
                diagnostics.push(diagnostic);
            }
        }

        diagnostics
    }

    /// Parse a single diagnostic line
    fn parse_diagnostic_line(&self, line: &str, uri: &Url) -> Option<Diagnostic> {
        use regex::Regex;

        // Regex for: "path/file.as:line:col: severity: message"
        let re = Regex::new(r"(.+):(\d+):(\d+):\s*(error|warning|hint|info|note):\s*(.+)").ok()?;
        let caps = re.captures(line)?;

        let line = caps.get(2)?.as_str().parse::<u32>().ok()?.saturating_sub(1);
        let col = caps.get(3)?.as_str().parse::<u32>().ok()?.saturating_sub(1);
        let severity_str = caps.get(4)?.as_str();
        let message = caps.get(5)?.as_str();

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
            code: None,
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

        // Run type checker
        let diagnostics = self.check_document(&uri, &text).await;

        // Publish diagnostics
        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        // Get the latest text from changes
        if let Some(change) = params.content_changes.last() {
            let text = &change.text;

            // Run type checker
            let diagnostics = self.check_document(&uri, text).await;

            // Publish diagnostics
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        // TODO: Phase 8 implementation
        // - [ ] Remove document from manager
        // - [ ] Clear diagnostics
        let _ = params;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        // TODO: Phase 8 implementation
        // - [ ] Find symbol at position
        // - [ ] Get type information
        // - [ ] Format hover content
        let _ = params;
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        // TODO: Phase 8 implementation
        // - [ ] Find symbol at position
        // - [ ] Look up definition location
        // - [ ] Return location
        let _ = params;
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        // TODO: Phase 8 implementation
        // - [ ] Find symbol at position
        // - [ ] Search for all references
        // - [ ] Return locations
        let _ = params;
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // TODO: Phase 8 implementation
        // - [ ] Determine completion context
        // - [ ] Generate candidates
        // - [ ] Filter and rank
        let _ = params;
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        // TODO: Phase 8 implementation
        // - [ ] Find symbol at position
        // - [ ] Find all references
        // - [ ] Generate workspace edit
        let _ = params;
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        // TODO: Phase 8 implementation
        // - [ ] Parse document
        // - [ ] Format AST
        // - [ ] Compute diff
        let _ = params;
        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        // TODO: Phase 8 implementation
        // - [ ] Get diagnostics for range
        // - [ ] Generate fix suggestions
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

    tracing::info!("Starting AffineScript Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

// TODO: Phase 8 implementation
// - [ ] Implement document manager with incremental updates
// - [ ] Integrate with AffineScript compiler for parsing/checking
// - [ ] Implement semantic tokens for syntax highlighting
// - [ ] Add inlay hints for types
// - [ ] Implement signature help
// - [ ] Add document symbols (outline)
// - [ ] Implement workspace symbol search
// - [ ] Add folding ranges
