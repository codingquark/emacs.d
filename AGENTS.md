# Repository Guidelines

## Project Structure & Module Organization
`config.org` is the source of truth for this Emacs setup. Add new packages, hooks, and helper functions there first. `init.el` stays minimal: it loads Org, tangles `config.org`, and holds the single `custom-set-variables` block. `config.el` is generated from `config.org` and should be committed alongside matching source changes so startup stays reproducible. Local runtime directories such as `elpa/`, `backups/`, and `auto-save-list/` are machine artifacts, not review targets.

## Build, Test, and Development Commands
Use Emacs itself as the build and validation tool:

- `emacs --batch --init-directory=. --eval '(message "startup ok")'` loads this checkout as the active config and catches startup errors.
- `emacs --batch --init-directory=. --eval "(progn (require 'org) (org-babel-tangle-file \"config.org\"))"` regenerates `config.el` from the literate source.
- `emacs --init-directory=.` launches an interactive session against this repo for manual verification.

Run the batch startup check before opening a PR. If you change Denote, Dired, Markdown, or Olivetti behavior, also test that workflow interactively.

**Batch mode limitations:** `--batch` runs without a display, so packages that depend on GUI features (AppleScript, D-Bus, frame parameters, system appearance) will not exercise those code paths. For such packages (e.g., `auto-dark`), prefer `:config` over `:init` in `use-package` blocks to defer activation until the package is fully loaded, and flag the change for interactive testing.

## Coding Style & Naming Conventions
Emacs Lisp here uses two-space indentation and spaces instead of tabs; the config explicitly sets `indent-tabs-mode` to `nil` and `tab-width` to `2`. Prefer `use-package` blocks and keep related settings grouped under `:init`, `:custom`, `:bind`, and `:config`. Use `:config` (not `:init`) for mode activation calls that trigger side effects like system calls or GUI operations. Local helper functions currently use the `cq-` prefix, and Lisp symbols should stay lower-case with hyphens. Keep comments brief and practical.

## Testing Guidelines
There is no dedicated automated test suite. Treat `emacs --batch --init-directory=. --eval '(message "startup ok")'` as the required smoke test. After feature changes, manually verify the affected mode or package, for example note creation in Denote or Markdown file association. Keep `config.org` and `config.el` synchronized in the same change.

## Commit & Pull Request Guidelines
Recent history favors short, imperative commit subjects such as `Update markdown command in Emacs configuration...`. Follow that style, mention the affected package or behavior, and avoid vague one-line jokes. Pull requests should summarize the user-visible change, list manual test steps, and call out machine-specific assumptions such as `muon.local`, font settings, or `~/Documents/notes`.
