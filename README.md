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

<https://github.com/user-attachments/assets/f084e652-b4f3-4f9c-a8e5-1cce6fb85e06>

## Installation

Build from source or install via cargo:

```bash
cargo install ebnfer
```

### Use w. Neovim (0.10.0)

Add ebnf as file type by adding the following to config: (`init.lua` fx.)

```lua
vim.filetype.add {
    extension = {
        ebnf = "ebnf",
    },
}
```

Add lsp attach with the following lua (N.B: cmd should have `.exe` suffix on windows):

```lua
vim.api.nvim_create_autocmd("FileType", {
    pattern = "ebnf",
    callback = function()
        vim.lsp.buf_attach_client(
            0,
            vim.lsp.start_client {
                name = "ebnfer",
                cmd = { "ebnfer" },
                on_attach = on_attach,
                capabilities = capabilities,
            }
        )
    end,
})
```

## Further development

- [ ] Vs Code extension - w. general document highlighting
- [ ] Workspace support - multiple files

## Inspired

This project is inspired by the ebnf analysis crate on crates.io - [ebnf](https://github.com/RubixDev/ebnf).
Also inspired by the following youtube video: [Learn By Building: Language Server Protocol - TJ Devries](https://www.youtube.com/watch?v=YsdlcQoHqPY).
