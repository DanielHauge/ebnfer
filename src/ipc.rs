pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::error::Error;

    use lsp_types::OneOf;
    use lsp_types::{
        request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
    };

    use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
    // https://github.com/rust-lang/rust-analyzer/blob/master/lib/lsp-server/examples/goto_def.rs
}
