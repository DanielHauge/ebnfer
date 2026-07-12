![Test](https://github.com/DanielHauge/ebnf-lsp/actions/workflows/rust.yml/badge.svg)
![crates.io](https://img.shields.io/crates/v/ebnfer.svg)

# EBNFER

An implementation of the language server protocol (LSP) for EBNF grammars.

## Features

- [X] Semantic tokens (Root rule)
- [x] Diagnostics
- [x] Hover
- [x] References
- [x] Completion
- [x] Document formatting
- [x] Rename
- [x] Go to definition
- [x] Document symbols
- [x] Code actions (supress unused warning)
- [x] Formatting
- [x] Workspace support - multiple files

<https://github.com/user-attachments/assets/f084e652-b4f3-4f9c-a8e5-1cce6fb85e06>

## Installation

Build from source or install via cargo:

```bash
cargo install ebnfer
```

### Visual Studio Code

Download the VSIX matching your platform and architecture from the GitHub
release, then install it:

```bash
code --install-extension ebnfer-<platform>.vsix
```

The extension includes the language server, EBNF syntax highlighting, and
editor configuration. Set `ebnfer.server.path` only when you want to override
the bundled executable.

### Neovim 0.11+

Install `ebnfer` with Cargo, then add the EBNF filetype and LSP configuration:

```lua
vim.filetype.add({
    extension = {
        ebnf = "ebnf",
    },
})

vim.lsp.config("ebnfer", {
    cmd = { vim.fn.has("win32") == 1 and "ebnfer.exe" or "ebnfer" },
    filetypes = { "ebnf" },
    root_markers = { ".ebnfer-root", ".git" },
})

vim.lsp.enable("ebnfer")
```

Use `:checkhealth vim.lsp` to confirm that the server attached. An optional
`.ebnfer-root` file can define the grammar workspace when the files are not
inside a Git repository.

## Workspaces

All `.ebnf` files below an LSP workspace folder are indexed recursively and
share one rule namespace. Definitions, references, hover, completion,
diagnostics, rename, and go-to-definition work across files.

Multi-root workspace folders remain independent namespaces. Open editor buffers
override their on-disk files until they are closed.

## Further development

- [ ] Publish the VS Code extension to the Marketplace and Open VSX

## Inspired

This project is inspired by the ebnf analysis crate on crates.io - [ebnf](https://github.com/RubixDev/ebnf).
Also inspired by the following youtube video: [Learn By Building: Language Server Protocol - TJ Devries](https://www.youtube.com/watch?v=YsdlcQoHqPY).
