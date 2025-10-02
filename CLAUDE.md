# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Emacs configuration directory (~/.emacs.d) using a combination of package managers:
- **straight.el** for bootstrapped package management
- **use-package** for declarative package configuration
- **package.el** with multiple archives (MELPA, org, non-gnu-elpa)

## Key Configuration Files

- **early-init.el**: Performance optimizations loaded before init.el (frame resizing, load preferences)
- **init.el**: Main configuration file - all package declarations and settings are here
- **manual-packages/claude-code**: Manually cloned claude-code.el package

## Important Configuration Details

### Package Management

The configuration uses straight.el (bootstrap version 7) as the primary package manager. Packages are configured using `use-package` declarations throughout init.el.

Manual packages are loaded from `~/.emacs.d/manual-packages/` and must be added to `load-path` before requiring.

### Terminal Backend

The configuration uses **eat** as the terminal backend for claude-code:
```elisp
(setq claude-code-terminal-backend 'eat)
```

This MUST be set before loading claude-code.

### Claude Code Integration

Claude Code is loaded manually from `~/.emacs.d/manual-packages/claude-code`:
1. Directory added to load-path
2. Backend set to 'eat before requiring
3. Loaded with `(require 'claude-code nil t)` - fails silently if not found
4. Keybinding: `C-c c` for `claude-code-command-map`

### Language Server Configuration

Uses **eglot** (not lsp-mode) as LSP client:
- C/C++ modes use clangd
- Python uses default eglot configuration
- Rust uses eglot via rustic package (`rustic-lsp-client 'eglot`)

### Python Environment

- Virtual environment: `/Users/rplace/src/alldyn/modules/modules`
- Indent offset: 2 spaces (not the Python standard 4)
- Uses pyvenv-mode for environment management
- Python completion uses company-jedi backend

### Completion Framework

Migrated from company to using both:
- **company-mode** (still active) for completion
- **cape** for additional completion backends
- **corfu** code is present but commented out

### Email Configuration

Uses mu4e for email with:
- mu binary: `/opt/homebrew/bin/mu`
- Mail directory: `~/Mail`
- Sync command: `mbsync --all`
- Three contexts: gmail-rwplace, alldyn, icloud
- SMTP via msmtp: `/opt/homebrew/bin/msmtp`

### Tree-sitter Support

Extensive tree-sitter language grammar configuration for bash, C, C++, cmake, elisp, go, HTML, JavaScript, JSON, Python, Rust, TypeScript, YAML, and more.

## Common Development Workflows

### Testing Configuration Changes

After modifying init.el:
1. Restart Emacs or evaluate changed sections
2. Check *Messages* buffer for errors
3. Use `M-x toggle-debug-on-error` for detailed error traces

### Adding New Packages

1. Add use-package declaration to init.el
2. Packages will auto-install via straight.el on next load
3. For manual packages: clone to `~/.emacs.d/manual-packages/` and add to load-path

### Shell Configuration

Default shell is zsh with:
- `/bin/zsh` as explicit shell
- Args: `--login --interactive`
- comint-process-echoes enabled in shell-mode

## Display and UI

- Font size: 16pt (height 160)
- Theme: modus-vivendi-deuteranopia (for red/green colorblindness)
- Line numbers globally enabled
- Uses vertico + marginalia for minibuffer completion
- Custom org-mode fonts with variable-pitch for prose, fixed-pitch for code/tables
