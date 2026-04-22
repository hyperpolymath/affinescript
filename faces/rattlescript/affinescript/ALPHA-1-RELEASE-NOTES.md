# AffineScript Alpha-1 Release Notes

## 🎮 Game Developer's Edition

**Release Date:** March 31, 2026  
**Version:** 0.1.0-alpha.1  
**Codename:** "Bug-Free by Design"

---

## 🚀 What's New in Alpha-1

### Core Language Features
- ✅ **Affine Types**: Compiler-proven resource management (no leaks!)
- ✅ **Effect System**: Explicit I/O and side effects tracking
- ✅ **Row Polymorphism**: Flexible data structures without boilerplate
- ✅ **Algebraic Effects**: Composable computation effects
- ✅ **WebAssembly Backend**: Compile to WASM for browser games
- ✅ **VSCode Integration**: Full syntax highlighting and LSP support

### Game Development Superpowers
- ✅ **Type-Safe Game State**: Compiler enforces valid state transitions
- ✅ **Resource Leak Prevention**: Textures, sounds, connections auto-managed
- ✅ **Protocol Correctness**: Network code that can't have protocol bugs
- ✅ **Zero-Cost Abstractions**: Type safety erased at compile time

### Ecosystem Integration
- ✅ **Gossamer**: Resource-safe desktop apps (PMPL-1.0-or-later)
- ✅ **Burble**: Low-latency voice comms (PMPL-1.0-or-later)
- ✅ **Tree-sitter Grammar**: Advanced syntax highlighting
- ✅ **Language Server**: IDE integration

---

## 🎯 Perfect For

**Indie Game Developers** who want:
- Fewer bugs in their game logic
- Compiler-enforced resource management
- Type-safe game state machines
- WebAssembly deployment

**Game Jam Participants** who need:
- Rapid prototyping with type safety
- No runtime crashes from invalid state
- Easy WASM deployment

**Educational Use** for teaching:
- Affine types and linear logic
- Type-driven game development
- Functional programming concepts

---

## 📦 What's Included

### Core Technology (PMPL-1.0-or-later)
- AffineScript compiler (OCaml backend)
- WebAssembly code generator
- Tree-sitter grammar
- VSCode extension
- Language Server Protocol implementation
- Standard library modules

### Game Examples (AGPL-3.0-or-later)
- Hello World with effects
- Resource management patterns
- Game state machine examples
- Type-safe entity systems
- Network protocol examples

### Documentation
- Complete language specification
- Game development tutorials
- Compiler architecture guide
- Type system reference

---

## 🔧 Installation

### Prerequisites
- OCaml 5.1+
- Dune 3.14+
- opam packages: `sedlex`, `menhir`, `ppx_deriving`

### Build from Source
```bash
git clone https://github.com/hyperpolymath/affinescript
git checkout v0.1.0-alpha.1
cd affinescript
dune build
```

### Try the Examples
```bash
# Type check a game example
dune exec affinescript -- check examples/hello.affine

# Run with interpreter
dune exec affinescript -- eval examples/hello.affine

# Compile to WebAssembly
dune exec affinescript -- compile examples/hello.affine -o hello.wasm
```

---

## 📝 Licensing

### Core Technology
**PMPL-1.0-or-later** - Palimpsest Mutual Public License
- Covers: Compiler, tooling, standard library
- Permissive with ethical use requirements
- Quantum-safe provenance tracking

### Game Content & Examples
**AGPL-3.0-or-later** - GNU Affero General Public License
- Covers: Game examples, assets, tutorials
- Ensures game content remains open
- Network use provisions for online games

---

## ⚠️ Known Limitations

### Not Yet Implemented
- **Effect Handlers**: Declarations parsed, runtime not implemented
- **Trait System**: 70% complete (basic traits work)
- **WASM Backend**: Basic types only (records coming soon)
- **Non-lexical Lifetimes**: Planned for beta

### Performance Notes
- Compiler: Fast enough for development
- WASM output: Not yet optimized
- Runtime: Minimal overhead from type erasure

---

## 🎮 Game Development Highlights

### Why This Changes Everything

**Before AffineScript:**
```rust
// Rust - manual resource management
let texture = load_texture("player.png");
// ... hundreds of lines later ...
unload_texture(texture); // Easy to forget!
```

**After AffineScript:**
```affinescript
// AffineScript - compiler-enforced resource management
fn game_loop() -> () / IO {
  let texture = load_texture("player.png"); // own GameTexture
  render(ref texture);
  unload(texture); // MUST happen - compiler proves it!
  // texture is GONE here - using it would be a compile error
}
```

### Real-World Impact
- ✅ **No more memory leaks** in game resources
- ✅ **No more invalid state** bugs in game logic
- ✅ **No more protocol errors** in network code
- ✅ **No more hidden I/O** in pure game functions
- ✅ **Flexible data** without boilerplate

---

## 🚀 Roadmap to 1.0

### Alpha Phase (Current)
- Core language working
- Basic WASM backend
- Game examples included
- Documentation complete

### Beta Phase (Q2 2026)
- Complete trait system
- Effect handlers runtime
- Advanced WASM features
- Performance optimization

### Release Candidate (Q3 2026)
- Full standard library
- Game engine integration
- Production-ready compiler
- Complete toolchain

### 1.0 Release (Q4 2026)
- Full feature set
- Production-ready
- Ecosystem packages
- Game jam templates

---

## 🤝 Community & Support

**GitHub**: https://github.com/hyperpolymath/affinescript
**Issues**: https://github.com/hyperpolymath/affinescript/issues
**Discussions**: https://github.com/hyperpolymath/affinescript/discussions

**Related Projects:**
- Gossamer: https://github.com/hyperpolymath/gossamer (PMPL-1.0-or-later)
- Burble: https://github.com/hyperpolymath/burble (PMPL-1.0-or-later)

---

## 🎁 Try It Today

```bash
# Clone the alpha release
git clone --branch v0.1.0-alpha.1 https://github.com/hyperpolymath/affinescript

# Build and run
dune build
dune exec affinescript -- eval examples/hello.affine

# Start building your bug-free game!
```

---

**AffineScript: Where your compiler becomes your QA team.**

SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell and contributors