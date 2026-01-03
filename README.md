# Techstack Enforcer

[![License: MPL-2.0](https://img.shields.io/badge/License-MPL_2.0-blue.svg)](https://opensource.org/licenses/MPL-2.0)
[![Philosophy: Palimpsest](https://img.shields.io/badge/Philosophy-Palimpsest-purple.svg)](https://github.com/hyperpolymath/palimpsest-licence)


A formally verified technology stack filter list enforcement system written in Ada/SPARK.

[![SPARK Verified](https://img.shields.io/badge/SPARK-verified-green)](https://www.adacore.com/about-spark)

## Overview

Techstack Enforcer blocks unwanted technologies across development layers with:

- **Multi-layer Enforcement**: Git hooks, CI/CD pipelines, file watchers
- **Formal Verification**: Core engine verified with SPARK
- **Definition Sets**: Pre-configured policies (strict, moderate, enterprise)
- **JSON API**: Structured decisions for automation
- **Interactive TUI**: Keyboard-driven filter management

## Quick Start

```bash
# Build
gprbuild -P techstack_enforcer.gpr -XMODE=release

# Audit repository
./bin/techstack_main audit .

# Use definition set
./bin/techstack_main --defset=strict audit .

# Get JSON decisions
./bin/techstack_main decide src/main.py

# Launch TUI
./bin/techstack_tui_main
```

## Definition Sets

| Set | Mode | Description |
|-----|------|-------------|
| `strict` | lockdown | Maximum memory safety |
| `moderate` | enforce | Balanced with exceptions |
| `permissive` | warn | Advisory only |
| `enterprise` | enforce | Corporate/legacy support |
| `memory_safe` | enforce | Focus on memory safety |

```bash
techstack-enforcer --defset=moderate audit .
techstack-enforcer defsets  # List all sets
```

## Commands

| Command | Description |
|---------|-------------|
| `audit <path>` | Full repository audit |
| `decide [path]` | JSON allow/deny decisions |
| `check <path>` | Check single file/directory |
| `list` | Show all filters |
| `defsets` | List definition sets |

## CI/CD Integration

**GitLab CI:**
```yaml
include:
  - project: 'your-group/techstack-filterlist'
    file: '/ci-templates/gitlab-ci-techstack.yml'

variables:
  TECHSTACK_DEFSET: moderate
```

**GitHub Actions:** See [ci-templates/github-action-techstack.yml](ci-templates/github-action-techstack.yml)

## Documentation

📚 **[Full Documentation](docs/README.md)**

- [Getting Started](docs/getting-started.md) - Installation and first steps
- [CLI Reference](docs/cli-reference.md) - Complete command reference
- [Definition Sets](docs/definition-sets.md) - Pre-configured policies
- [TUI Guide](docs/tui-guide.md) - Interactive interface
- [CI Integration](docs/ci-integration.md) - Pipeline setup
- [API & JSON Format](docs/api-json-format.md) - Automation interface

## Architecture

```
src/
├── core/                    # SPARK-verified enforcement engine
│   ├── techstack_types.ads      # Type definitions
│   ├── techstack_enforcer.ads   # Core logic
│   ├── techstack_patterns.ads   # Glob matching
│   ├── techstack_defsets.ads    # Definition sets
│   └── techstack_json_io.ads    # JSON API
├── tui/                     # Terminal interface
├── learning/                # Security pattern analysis
└── hooks/                   # Git hook integration
```

## Filter Levels

| Level | Symbol | Behavior |
|-------|--------|----------|
| Fatal | `#` | Always blocked |
| Block | `X` | Blocked in enforce/lockdown |
| Warn | `!` | Warning only |
| Allow | `+` | Explicitly permitted |

## Philosophy

This tool enforces preferences for:

1. **Memory-safe languages** (Rust, Ada/SPARK, Haskell)
2. **Static typing** (TypeScript, Rust)
3. **Formal verification** (Ada/SPARK)
4. **Open infrastructure** (Podman, GitLab)

## Building

Requirements: GNAT, GPRbuild

```bash
# Debug build
gprbuild -P techstack_enforcer.gpr -XMODE=debug

# Release build
gprbuild -P techstack_enforcer.gpr -XMODE=release

# SPARK verification
gnatprove -P techstack_enforcer.gpr --level=2
```

## License

MIT
