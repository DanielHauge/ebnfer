# EBNFER for Visual Studio Code

Language support for Extended Backus-Naur Form (`.ebnf`) files.

The extension bundles the `ebnfer` language server and provides syntax
highlighting, diagnostics, completion, hover, references, rename, formatting,
and cross-file workspace navigation.

## Server override

The platform-specific VSIX includes the matching server binary. For development
or troubleshooting, set `ebnfer.server.path` to another executable.

## Installation

Download the VSIX matching your operating system and architecture, then run:

```console
code --install-extension ebnfer-<platform>.vsix
```

Release tags must match both `Cargo.toml` and this extension's `package.json`.
The release workflow produces six target-specific VSIX files. Upload the full
set together when publishing manually to the Visual Studio Marketplace.

## Development

```console
npm ci
npm test
cargo build --release --manifest-path ../../Cargo.toml
node scripts/stage-server.mjs ../../target/release/ebnfer
npm run test:integration
```

To create a target-specific package after building the matching Rust binary:

```console
npm run package:target -- linux-x64 ../../target/x86_64-unknown-linux-musl/release/ebnfer
```
