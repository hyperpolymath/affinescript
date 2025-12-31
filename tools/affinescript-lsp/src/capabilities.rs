// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Server Capabilities
//!
//! Defines what features the language server supports.

use tower_lsp::lsp_types::*;

/// Build the server capabilities to advertise to the client
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // Text document sync
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),

        // Hover
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        // Completion
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            resolve_provider: Some(true),
            ..Default::default()
        }),

        // Definition
        definition_provider: Some(OneOf::Left(true)),

        // References
        references_provider: Some(OneOf::Left(true)),

        // Document highlight (same-symbol highlighting)
        document_highlight_provider: Some(OneOf::Left(true)),

        // Document symbols (outline)
        document_symbol_provider: Some(OneOf::Left(true)),

        // Workspace symbol search
        workspace_symbol_provider: Some(OneOf::Left(true)),

        // Code actions (quick fixes)
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),

        // Formatting
        document_formatting_provider: Some(OneOf::Left(true)),

        // Range formatting
        document_range_formatting_provider: Some(OneOf::Left(true)),

        // Rename
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),

        // Folding
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),

        // Signature help
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),

        // Semantic tokens (syntax highlighting)
        semantic_tokens_provider: Some(
            SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: semantic_token_types(),
                    token_modifiers: semantic_token_modifiers(),
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: Some(true),
                ..Default::default()
            }),
        ),

        // Inlay hints (type annotations)
        inlay_hint_provider: Some(OneOf::Left(true)),

        // Workspace capabilities
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: None,
        }),

        ..Default::default()
    }
}

/// Semantic token types for syntax highlighting
fn semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::MACRO,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::OPERATOR,
        // AffineScript-specific
        SemanticTokenType::new("effect"),
        SemanticTokenType::new("handler"),
        SemanticTokenType::new("quantity"),
        SemanticTokenType::new("lifetime"),
    ]
}

/// Semantic token modifiers
fn semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::STATIC,
        SemanticTokenModifier::DEPRECATED,
        SemanticTokenModifier::ABSTRACT,
        SemanticTokenModifier::ASYNC,
        SemanticTokenModifier::MODIFICATION,
        SemanticTokenModifier::DOCUMENTATION,
        SemanticTokenModifier::DEFAULT_LIBRARY,
        // AffineScript-specific
        SemanticTokenModifier::new("linear"),
        SemanticTokenModifier::new("affine"),
        SemanticTokenModifier::new("unrestricted"),
        SemanticTokenModifier::new("erased"),
        SemanticTokenModifier::new("mutable"),
        SemanticTokenModifier::new("borrowed"),
    ]
}
