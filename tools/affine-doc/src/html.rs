// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! HTML Generation
//!
//! Generates HTML documentation pages.

use crate::extract::{ItemDoc, ItemKind, ModuleDoc};
use crate::render;
use std::path::Path;

/// HTML generator configuration
#[derive(Debug, Clone)]
pub struct HtmlConfig {
    /// Output directory
    pub output_dir: std::path::PathBuf,

    /// Package name
    pub package_name: String,

    /// Package version
    pub package_version: String,

    /// Custom CSS
    pub custom_css: Option<String>,

    /// Theme (light, dark, auto)
    pub theme: String,
}

/// HTML generator
pub struct HtmlGenerator {
    config: HtmlConfig,
}

impl HtmlGenerator {
    /// Create a new HTML generator
    pub fn new(config: HtmlConfig) -> Self {
        HtmlGenerator { config }
    }

    /// Generate documentation for all modules
    pub fn generate(&self, modules: &[ModuleDoc]) -> anyhow::Result<()> {
        // Create output directory
        std::fs::create_dir_all(&self.config.output_dir)?;

        // Generate static assets
        self.generate_assets()?;

        // Generate index page
        self.generate_index(modules)?;

        // Generate module pages
        for module in modules {
            self.generate_module(module)?;
        }

        // Generate search index
        self.generate_search_index(modules)?;

        Ok(())
    }

    /// Generate static assets (CSS, JS)
    fn generate_assets(&self) -> anyhow::Result<()> {
        let css = include_str!("../assets/style.css");
        let js = include_str!("../assets/search.js");

        std::fs::write(self.config.output_dir.join("style.css"), css)?;
        std::fs::write(self.config.output_dir.join("search.js"), js)?;

        Ok(())
    }

    /// Generate index page
    fn generate_index(&self, modules: &[ModuleDoc]) -> anyhow::Result<()> {
        let mut html = self.page_header("Index");

        html.push_str("<main class=\"index\">");
        html.push_str(&format!("<h1>{}</h1>", self.config.package_name));
        html.push_str(&format!(
            "<p class=\"version\">Version {}</p>",
            self.config.package_version
        ));

        html.push_str("<h2>Modules</h2>");
        html.push_str("<ul class=\"module-list\">");
        for module in modules {
            let path = module.path.join("::");
            html.push_str(&format!(
                "<li><a href=\"{}.html\">{}</a></li>",
                path.replace("::", "/"),
                path
            ));
        }
        html.push_str("</ul>");

        html.push_str("</main>");
        html.push_str(&self.page_footer());

        std::fs::write(self.config.output_dir.join("index.html"), html)?;
        Ok(())
    }

    /// Generate module documentation
    fn generate_module(&self, module: &ModuleDoc) -> anyhow::Result<()> {
        let module_path = module.path.join("::");
        let mut html = self.page_header(&module_path);

        html.push_str("<main class=\"module\">");
        html.push_str(&format!("<h1>Module {}</h1>", module_path));

        if let Some(doc) = &module.doc {
            html.push_str("<div class=\"module-doc\">");
            html.push_str(&render::render_markdown(doc));
            html.push_str("</div>");
        }

        // Group items by kind
        let mut functions = vec![];
        let mut types = vec![];
        let mut traits = vec![];
        let mut effects = vec![];

        for item in &module.items {
            match item.kind {
                ItemKind::Function => functions.push(item),
                ItemKind::Struct | ItemKind::Enum | ItemKind::TypeAlias => types.push(item),
                ItemKind::Trait => traits.push(item),
                ItemKind::Effect => effects.push(item),
                _ => {}
            }
        }

        // Render sections
        if !types.is_empty() {
            html.push_str(&self.render_section("Types", &types));
        }
        if !traits.is_empty() {
            html.push_str(&self.render_section("Traits", &traits));
        }
        if !effects.is_empty() {
            html.push_str(&self.render_section("Effects", &effects));
        }
        if !functions.is_empty() {
            html.push_str(&self.render_section("Functions", &functions));
        }

        html.push_str("</main>");
        html.push_str(&self.page_footer());

        // Write file
        let file_path = self
            .config
            .output_dir
            .join(format!("{}.html", module_path.replace("::", "/")));

        if let Some(parent) = file_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(file_path, html)?;

        Ok(())
    }

    /// Render a section of items
    fn render_section(&self, title: &str, items: &[&ItemDoc]) -> String {
        let mut html = format!("<section class=\"items\">");
        html.push_str(&format!("<h2>{}</h2>", title));

        for item in items {
            html.push_str(&self.render_item(item));
        }

        html.push_str("</section>");
        html
    }

    /// Render a single item
    fn render_item(&self, item: &ItemDoc) -> String {
        let mut html = format!(
            "<div class=\"item {}\" id=\"{}\">",
            item.kind.display_name().to_lowercase(),
            item.name
        );

        // Header
        html.push_str("<div class=\"item-header\">");
        html.push_str(&format!(
            "<span class=\"kind\">{}</span>",
            item.kind.display_name()
        ));
        html.push_str(&format!("<code class=\"name\">{}</code>", item.name));
        html.push_str("</div>");

        // Signature
        html.push_str("<pre class=\"signature\">");
        html.push_str(&render::html_escape(&item.signature));
        html.push_str("</pre>");

        // Documentation
        if let Some(doc) = &item.doc {
            html.push_str("<div class=\"item-doc\">");
            html.push_str(&render::render_markdown(doc));
            html.push_str("</div>");
        }

        // Examples
        for example in &item.examples {
            html.push_str("<div class=\"example\">");
            html.push_str("<h4>Example</h4>");
            html.push_str(&render::render_code(example, Some("affinescript")));
            html.push_str("</div>");
        }

        html.push_str("</div>");
        html
    }

    /// Generate page header
    fn page_header(&self, title: &str) -> String {
        format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{} - {} Documentation</title>
    <link rel="stylesheet" href="/style.css">
</head>
<body class="theme-{}">
    <nav class="sidebar">
        <div class="search">
            <input type="text" id="search" placeholder="Search...">
        </div>
    </nav>
"#,
            title, self.config.package_name, self.config.theme
        )
    }

    /// Generate page footer
    fn page_footer(&self) -> String {
        r#"
    <script src="/search.js"></script>
</body>
</html>
"#
        .to_string()
    }

    /// Generate search index
    fn generate_search_index(&self, _modules: &[ModuleDoc]) -> anyhow::Result<()> {
        // TODO: Phase 8 implementation
        // - [ ] Extract searchable content
        // - [ ] Build JSON index
        // - [ ] Or build Tantivy index

        Ok(())
    }
}

// TODO: Phase 8 implementation
// - [ ] Add navigation sidebar
// - [ ] Add breadcrumbs
// - [ ] Add source links
// - [ ] Add copy permalink
// - [ ] Add implementor lists for traits
// - [ ] Add method lists for types
// - [ ] Support custom templates
