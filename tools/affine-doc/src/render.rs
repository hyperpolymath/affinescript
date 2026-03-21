// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Markdown Rendering
//!
//! Renders Markdown doc comments to HTML.

use pulldown_cmark::{html, Options, Parser};

/// Render Markdown to HTML
pub fn render_markdown(markdown: &str) -> String {
    let options = Options::all();
    let parser = Parser::new_ext(markdown, options);

    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    html_output
}

/// Render code with syntax highlighting
pub fn render_code(code: &str, language: Option<&str>) -> String {
    // TODO: Phase 8 implementation
    // - [ ] Use syntect for highlighting
    // - [ ] Support AffineScript syntax
    // - [ ] Add line numbers option

    let lang = language.unwrap_or("affinescript");
    format!(
        r#"<pre><code class="language-{}">{}</code></pre>"#,
        lang,
        html_escape(code)
    )
}

/// Escape HTML entities
pub fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

/// Render a type signature with links
pub fn render_signature(signature: &str, _link_resolver: &dyn Fn(&str) -> Option<String>) -> String {
    // TODO: Phase 8 implementation
    // - [ ] Parse signature
    // - [ ] Identify type names
    // - [ ] Create links to type documentation
    // - [ ] Apply syntax highlighting

    format!(
        r#"<code class="signature">{}</code>"#,
        html_escape(signature)
    )
}

/// Render effect row
pub fn render_effects(effects: &[String]) -> String {
    if effects.is_empty() {
        return String::new();
    }

    let effect_links: Vec<String> = effects
        .iter()
        .map(|e| format!(r#"<a href="#{}" class="effect">{}</a>"#, e, e))
        .collect();

    format!(r#"<span class="effects">/ {}</span>"#, effect_links.join(" | "))
}

/// Render quantity annotation
pub fn render_quantity(quantity: &str) -> String {
    let (class, display) = match quantity {
        "0" => ("quantity-erased", "0"),
        "1" => ("quantity-linear", "1"),
        "ω" | "w" | "omega" => ("quantity-unrestricted", "ω"),
        _ => ("quantity-unknown", quantity),
    };

    format!(r#"<span class="{}">{}</span>"#, class, display)
}

/// Render deprecation notice
pub fn render_deprecated(message: Option<&str>) -> String {
    match message {
        Some(msg) => format!(
            r#"<div class="deprecated"><strong>Deprecated:</strong> {}</div>"#,
            html_escape(msg)
        ),
        None => r#"<div class="deprecated"><strong>Deprecated</strong></div>"#.to_string(),
    }
}

/// Render stability badge
pub fn render_stability(stability: Stability) -> String {
    let (class, label) = match stability {
        Stability::Stable => ("stability-stable", "Stable"),
        Stability::Unstable => ("stability-unstable", "Unstable"),
        Stability::Experimental => ("stability-experimental", "Experimental"),
        Stability::Deprecated => ("stability-deprecated", "Deprecated"),
    };

    format!(r#"<span class="badge {}">{}</span>"#, class, label)
}

/// Stability level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stability {
    Stable,
    Unstable,
    Experimental,
    Deprecated,
}

// TODO: Phase 8 implementation
// - [ ] Add syntax highlighting for AffineScript
// - [ ] Implement cross-reference resolution
// - [ ] Add heading anchor links
// - [ ] Support custom markdown extensions
// - [ ] Add copy button for code blocks
