// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Content-Addressed Storage
//!
//! Stores packages by content hash for deduplication (like pnpm).

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

/// Content store directory
pub const STORE_DIR: &str = ".affine/store";

/// Package store
pub struct PackageStore {
    /// Store root directory
    root: PathBuf,
}

impl PackageStore {
    /// Create or open a package store
    pub fn new(root: impl AsRef<Path>) -> std::io::Result<Self> {
        let root = root.as_ref().to_path_buf();
        std::fs::create_dir_all(&root)?;
        Ok(PackageStore { root })
    }

    /// Get the default store location
    pub fn default_store() -> std::io::Result<Self> {
        let home = dirs::home_dir()
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "no home directory"))?;

        Self::new(home.join(STORE_DIR))
    }

    /// Store content and return its hash
    pub fn store(&self, content: &[u8]) -> std::io::Result<ContentHash> {
        let hash = ContentHash::compute(content);
        let path = self.path_for(&hash);

        if !path.exists() {
            // Create parent directories
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }

            // Write atomically
            let temp_path = path.with_extension("tmp");
            std::fs::write(&temp_path, content)?;
            std::fs::rename(&temp_path, &path)?;
        }

        Ok(hash)
    }

    /// Retrieve content by hash
    pub fn retrieve(&self, hash: &ContentHash) -> std::io::Result<Vec<u8>> {
        let path = self.path_for(hash);
        std::fs::read(path)
    }

    /// Check if content exists
    pub fn contains(&self, hash: &ContentHash) -> bool {
        self.path_for(hash).exists()
    }

    /// Get path for a content hash
    pub fn path_for(&self, hash: &ContentHash) -> PathBuf {
        // Use first 2 chars as directory for sharding
        let hex = hash.to_hex();
        self.root.join(&hex[..2]).join(&hex[2..])
    }

    /// Store a package and create a link
    pub fn store_package(
        &self,
        name: &str,
        version: &str,
        content: &[u8],
    ) -> std::io::Result<PathBuf> {
        let hash = self.store(content)?;

        // Create package directory
        let pkg_dir = self.root.join("packages").join(name).join(version);
        std::fs::create_dir_all(&pkg_dir)?;

        // Extract tarball
        // TODO: Phase 8 implementation
        // - [ ] Extract tar.gz to pkg_dir
        // - [ ] Create integrity file

        Ok(pkg_dir)
    }

    /// Link package to node_modules equivalent
    pub fn link_package(
        &self,
        _name: &str,
        _version: &str,
        _target: impl AsRef<Path>,
    ) -> std::io::Result<()> {
        // TODO: Phase 8 implementation
        // - [ ] Create symlink or copy on Windows
        // - [ ] Handle nested dependencies

        Ok(())
    }

    /// Garbage collect unused content
    pub fn gc(&self) -> std::io::Result<GcStats> {
        // TODO: Phase 8 implementation
        // - [ ] Scan all projects for used packages
        // - [ ] Remove unreferenced content
        // - [ ] Return statistics

        Ok(GcStats {
            removed_count: 0,
            removed_bytes: 0,
        })
    }

    /// Verify store integrity
    pub fn verify(&self) -> std::io::Result<Vec<VerifyError>> {
        // TODO: Phase 8 implementation
        // - [ ] Scan all content
        // - [ ] Verify hashes match
        // - [ ] Report errors

        Ok(vec![])
    }
}

/// Content hash (SHA-256)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContentHash([u8; 32]);

impl ContentHash {
    /// Compute hash of content
    pub fn compute(content: &[u8]) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(content);
        let result = hasher.finalize();
        ContentHash(result.into())
    }

    /// Parse from hex string
    pub fn from_hex(hex: &str) -> Result<Self, hex::FromHexError> {
        let bytes = hex::decode(hex)?;
        if bytes.len() != 32 {
            return Err(hex::FromHexError::InvalidStringLength);
        }
        let mut arr = [0u8; 32];
        arr.copy_from_slice(&bytes);
        Ok(ContentHash(arr))
    }

    /// Convert to hex string
    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }
}

impl std::fmt::Display for ContentHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sha256:{}", self.to_hex())
    }
}

/// GC statistics
#[derive(Debug, Default)]
pub struct GcStats {
    /// Number of items removed
    pub removed_count: usize,

    /// Bytes freed
    pub removed_bytes: u64,
}

/// Verification error
#[derive(Debug)]
pub struct VerifyError {
    /// Path to corrupted content
    pub path: PathBuf,

    /// Expected hash
    pub expected: ContentHash,

    /// Actual hash
    pub actual: ContentHash,
}

// TODO: Phase 8 implementation
// - [ ] Add parallel extraction
// - [ ] Add hardlink support for same-OS
// - [ ] Add copy-on-write support (reflinks)
// - [ ] Add lockfile for concurrent access
// - [ ] Add prune for specific packages
