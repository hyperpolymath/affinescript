// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Configuration
//!
//! User and project configuration for the package manager.

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Global configuration file location
pub fn global_config_path() -> Option<PathBuf> {
    dirs::config_dir().map(|d| d.join("affine").join("config.toml"))
}

/// Credentials file location
pub fn credentials_path() -> Option<PathBuf> {
    dirs::config_dir().map(|d| d.join("affine").join("credentials.toml"))
}

/// Global configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Config {
    /// Registry configuration
    #[serde(default)]
    pub registries: Vec<RegistryConfig>,

    /// Network configuration
    #[serde(default)]
    pub net: NetworkConfig,

    /// Build configuration
    #[serde(default)]
    pub build: BuildConfig,

    /// Environment variables
    #[serde(default)]
    pub env: std::collections::HashMap<String, String>,
}

/// Registry configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryConfig {
    /// Registry name
    pub name: String,

    /// Registry URL
    pub url: String,

    /// Whether this is the default registry
    #[serde(default)]
    pub default: bool,
}

/// Network configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NetworkConfig {
    /// HTTP proxy
    pub proxy: Option<String>,

    /// HTTPS proxy
    pub https_proxy: Option<String>,

    /// No proxy hosts
    #[serde(default)]
    pub no_proxy: Vec<String>,

    /// Connection timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout: u64,

    /// Number of retries
    #[serde(default = "default_retries")]
    pub retries: u32,

    /// Whether to verify SSL certificates
    #[serde(default = "default_true")]
    pub verify_ssl: bool,
}

fn default_timeout() -> u64 {
    30
}

fn default_retries() -> u32 {
    3
}

fn default_true() -> bool {
    true
}

/// Build configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BuildConfig {
    /// Number of parallel jobs
    pub jobs: Option<usize>,

    /// Target directory
    pub target_dir: Option<PathBuf>,

    /// Whether to use incremental compilation
    #[serde(default = "default_true")]
    pub incremental: bool,

    /// Compiler flags
    #[serde(default)]
    pub flags: Vec<String>,
}

/// Credentials
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Credentials {
    /// Token by registry name
    pub tokens: std::collections::HashMap<String, String>,
}

impl Config {
    /// Load global configuration
    pub fn load_global() -> anyhow::Result<Self> {
        match global_config_path() {
            Some(path) if path.exists() => {
                let content = std::fs::read_to_string(&path)?;
                Ok(toml::from_str(&content)?)
            }
            _ => Ok(Self::default()),
        }
    }

    /// Save global configuration
    pub fn save_global(&self) -> anyhow::Result<()> {
        if let Some(path) = global_config_path() {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let content = toml::to_string_pretty(self)?;
            std::fs::write(path, content)?;
        }
        Ok(())
    }

    /// Load project-local configuration
    pub fn load_local(project_root: impl AsRef<Path>) -> anyhow::Result<Option<Self>> {
        let path = project_root.as_ref().join(".affine").join("config.toml");
        if path.exists() {
            let content = std::fs::read_to_string(&path)?;
            Ok(Some(toml::from_str(&content)?))
        } else {
            Ok(None)
        }
    }

    /// Merge with another config (other takes precedence)
    pub fn merge(&mut self, other: Self) {
        if !other.registries.is_empty() {
            self.registries = other.registries;
        }
        // Merge network config
        if other.net.proxy.is_some() {
            self.net.proxy = other.net.proxy;
        }
        if other.net.https_proxy.is_some() {
            self.net.https_proxy = other.net.https_proxy;
        }
        // Merge build config
        if other.build.jobs.is_some() {
            self.build.jobs = other.build.jobs;
        }
        if other.build.target_dir.is_some() {
            self.build.target_dir = other.build.target_dir;
        }
        // Merge env
        self.env.extend(other.env);
    }

    /// Get the default registry URL
    pub fn default_registry(&self) -> String {
        self.registries
            .iter()
            .find(|r| r.default)
            .map(|r| r.url.clone())
            .unwrap_or_else(|| crate::registry::DEFAULT_REGISTRY.to_string())
    }
}

impl Credentials {
    /// Load credentials
    pub fn load() -> anyhow::Result<Self> {
        match credentials_path() {
            Some(path) if path.exists() => {
                let content = std::fs::read_to_string(&path)?;
                Ok(toml::from_str(&content)?)
            }
            _ => Ok(Self::default()),
        }
    }

    /// Save credentials
    pub fn save(&self) -> anyhow::Result<()> {
        if let Some(path) = credentials_path() {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            // Set restrictive permissions on credentials file
            let content = toml::to_string_pretty(self)?;
            std::fs::write(&path, content)?;

            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mut perms = std::fs::metadata(&path)?.permissions();
                perms.set_mode(0o600);
                std::fs::set_permissions(&path, perms)?;
            }
        }
        Ok(())
    }

    /// Get token for a registry
    pub fn get_token(&self, registry: &str) -> Option<&str> {
        self.tokens.get(registry).map(|s| s.as_str())
    }

    /// Set token for a registry
    pub fn set_token(&mut self, registry: String, token: String) {
        self.tokens.insert(registry, token);
    }
}

// TODO: Phase 8 implementation
// - [ ] Add environment variable overrides
// - [ ] Add configuration validation
// - [ ] Add shell completion config
// - [ ] Add alias support
// - [ ] Add profiles (dev, release, etc.)
