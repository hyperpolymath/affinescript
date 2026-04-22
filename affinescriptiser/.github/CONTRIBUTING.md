# Clone the repository
git clone https://github.com/hyperpolymath/affinescriptiser.git
cd affinescriptiser

# Using Nix (recommended for reproducibility)
nix develop

# Or using toolbox/distrobox
toolbox create affinescriptiser-dev
toolbox enter affinescriptiser-dev
# Install dependencies manually

# Verify setup
just check   # or: cargo check / mix compile / etc.
just test    # Run test suite
```

### Repository Structure
```
affinescriptiser/
├── src/                 # Source code (Perimeter 1-2)
├── lib/                 # Library code (Perimeter 1-2)
├── extensions/          # Extensions (Perimeter 2)
├── plugins/             # Plugins (Perimeter 2)
├── tools/               # Tooling (Perimeter 2)
├── docs/                # Documentation (Perimeter 3)
│   ├── architecture/    # ADRs, specs (Perimeter 2)
│   └── proposals/       # RFCs (Perimeter 3)
├── examples/            # Examples (Perimeter 3)
├── spec/                # Spec tests (Perimeter 3)
├── tests/               # Test suite (Perimeter 2-3)
├── .machine_readable/   # ALL machine-readable content (Perimeter 1)
│   ├── *.a2ml           # State files (STATE, META, ECOSYSTEM, etc.)
│   ├── bot_directives/  # Bot configs
│   └── contractiles/    # Policy contracts (k9, dust, lust, must, trust)
├── .well-known/         # Protocol files (Perimeter 1-3)
├── .github/             # GitHub config (Perimeter 1)
│   ├── ISSUE_TEMPLATE/
│   └── workflows/
├── CHANGELOG.md
├── CODE_OF_CONDUCT.md
├── CONTRIBUTING.md      # This file
├── GOVERNANCE.md
├── LICENSE
├── MAINTAINERS.md
├── README.adoc
├── SECURITY.md
├── flake.nix            # Nix flake — fallback (Perimeter 1)
├── guix.scm             # Guix package — primary (Perimeter 1)
└── Justfile             # Task runner (Perimeter 1)
```

---

## How to Contribute

### Reporting Bugs

**Before reporting**:
1. Search existing issues
2. Check if it's already fixed in `{{MAIN_BRANCH}}`
3. Determine which perimeter the bug affects

**When reporting**:

Use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md) and include:

- Clear, descriptive title
- Environment details (OS, versions, toolchain)
- Steps to reproduce
- Expected vs actual behaviour
- Logs, screenshots, or minimal reproduction

### Suggesting Features

**Before suggesting**:
1. Check the [roadmap](ROADMAP.md) if available
2. Search existing issues and discussions
3. Consider which perimeter the feature belongs to

**When suggesting**:

Use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md) and include:

- Problem statement (what pain point does this solve?)
- Proposed solution
- Alternatives considered
- Which perimeter this affects

### Your First Contribution

Look for issues labelled:

- [`good first issue`](https://github.com/hyperpolymath/affinescriptiser/labels/good%20first%20issue) — Simple Perimeter 3 tasks
- [`help wanted`](https://github.com/hyperpolymath/affinescriptiser/labels/help%20wanted) — Community help needed
- [`documentation`](https://github.com/hyperpolymath/affinescriptiser/labels/documentation) — Docs improvements
- [`perimeter-3`](https://github.com/hyperpolymath/affinescriptiser/labels/perimeter-3) — Community sandbox scope

---

## Development Workflow

### Branch Naming
```
docs/short-description       # Documentation (P3)
test/what-added              # Test additions (P3)
feat/short-description       # New features (P2)
fix/issue-number-description # Bug fixes (P2)
refactor/what-changed        # Code improvements (P2)
security/what-fixed          # Security fixes (P1-2)
```

### Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/):
```
<type>(<scope>): <description>

[optional body]

[optional footer]
