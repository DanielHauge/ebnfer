![Test](https://github.com/DanielHauge/ebnf-lsp/actions/workflows/rust.yml/badge.svg)

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

## Installation

Build from source or install via cargo:

```bash
cargo install ebnfer
```

## Further development

- [ ] Vs Code extension - w. general document highlighting
- [ ] Workspace support - multiple files

## Inspired

This project is inspired by the ebnf analysis crate on crates.io - [ebnf](https://github.com/RubixDev/ebnf).
Also inspired by the following youtube video: [Learn By Building: Language Server Protocol - TJ Devries](https://www.youtube.com/watch?v=YsdlcQoHqPY).
