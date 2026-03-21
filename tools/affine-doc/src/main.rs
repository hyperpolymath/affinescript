// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! AffineScript Documentation Generator
//!
//! Generates API documentation from AffineScript source code.
//!
//! # Features
//!
//! - Extracts doc comments from source
//! - Generates HTML documentation
//! - Creates search index
//! - Supports cross-linking
//! - Renders Markdown in comments

use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod extract;
mod html;
mod index;
mod render;

#[derive(Parser)]
#[command(name = "affine-doc")]
#[command(about = "AffineScript documentation generator", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate documentation
    Build {
        /// Source directory
        #[arg(default_value = ".")]
        source: PathBuf,

        /// Output directory
        #[arg(short, long, default_value = "target/doc")]
        output: PathBuf,

        /// Open in browser after building
        #[arg(long)]
        open: bool,

        /// Include private items
        #[arg(long)]
        document_private: bool,

        /// Include dependencies
        #[arg(long)]
        include_deps: bool,
    },

    /// Start documentation server
    #[cfg(feature = "serve")]
    Serve {
        /// Documentation directory
        #[arg(default_value = "target/doc")]
        dir: PathBuf,

        /// Port to listen on
        #[arg(short, long, default_value = "8080")]
        port: u16,
    },

    /// Generate search index only
    Index {
        /// Documentation directory
        #[arg(default_value = "target/doc")]
        dir: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Build {
            source,
            output,
            open,
            document_private,
            include_deps,
        } => {
            // TODO: Phase 8 implementation
            // - [ ] Parse source files
            // - [ ] Extract documentation
            // - [ ] Generate HTML
            // - [ ] Build search index
            // - [ ] Open browser if requested

            println!("Building documentation from {:?} to {:?}", source, output);
            let _ = (document_private, include_deps, open);
        }

        #[cfg(feature = "serve")]
        Commands::Serve { dir, port } => {
            // TODO: Phase 8 implementation
            // - [ ] Start HTTP server
            // - [ ] Serve static files
            // - [ ] Handle search API

            println!("Serving documentation from {:?} on port {}", dir, port);
        }

        Commands::Index { dir } => {
            // TODO: Phase 8 implementation
            // - [ ] Scan HTML files
            // - [ ] Extract content
            // - [ ] Build Tantivy index

            println!("Building search index for {:?}", dir);
        }
    }

    Ok(())
}

// TODO: Phase 8 implementation
// - [ ] Parse AffineScript source and extract types/functions
// - [ ] Process doc comments (Markdown)
// - [ ] Generate HTML with templates
// - [ ] Create type/effect cross-references
// - [ ] Build searchable index
// - [ ] Support theme customization
// - [ ] Add source view with syntax highlighting
