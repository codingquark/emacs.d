# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Also read `AGENTS.md` for additional instructions.

## Overview

This is codingquark's personal Emacs configuration, using a literate programming approach via Org mode.

## Architecture

- **`init.el`**: Bootstrap file. Loads `config.org` via `org-babel-load-file`. Also contains Emacs Custom variables (auto-generated). Do not manually edit the `custom-set-variables` / `custom-set-faces` blocks.
- **`config.org`**: The canonical source of configuration. All Elisp code blocks in this file are tangled to produce `config.el`. Edit this file for configuration changes.
- **`config.el`**: Auto-generated from `config.org` by org-babel. Do not edit directly — changes will be overwritten.
- **`elpa/`**: Package directory (git-ignored). Packages install automatically via `use-package` with `ensure t`.

## Key conventions

- Packages sourced from MELPA Stable (not regular MELPA)
- All packages use `use-package` with `:ensure t` globally enabled
- Custom functions are prefixed with `cq-` (e.g., `cq-open-denote-directory`, `cq-insert-time-stamp`)
- Machine-specific config uses `system-name` checks (e.g., `"muon.local"` for font settings)
- Denote keybindings live under the `C-c n` prefix

## When making changes

1. Edit `config.org`, not `config.el`
2. Keep `config.el` in sync — after editing `config.org`, regenerate by running `M-x org-babel-tangle` in Emacs, or simply restart Emacs (init.el runs `org-babel-load-file` on startup)
3. When adding packages, also add them to the `package-selected-packages` list in `init.el` under `custom-set-variables`

## Installed packages

modus-themes, denote, denote-menu, denote-markdown, denote-journal, olivetti, auto-dark, markdown-mode
