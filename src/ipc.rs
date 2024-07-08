pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::collections::HashMap;
    use std::error::Error;
    use std::panic;
    use std::rc::Rc;

    use lsp_types::notification::{
        self, DidOpenNotebookDocument, DidOpenTextDocument, Notification,
    };
    use lsp_types::request::HoverRequest;
    use lsp_types::{
        request::GotoDefinition, DiagnosticOptions, DiagnosticServerCapabilities,
        GotoDefinitionResponse, InitializeParams, ServerCapabilities,
    };
    use lsp_types::{
        DidOpenTextDocumentParams, Hover, HoverProviderCapability, OneOf,
        TextDocumentSyncCapability, TextDocumentSyncKind,
    };

    use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
    use serde_json::Value;

    use crate::ebnf::ebnf::parse_ebnf;
    use crate::lsp::lsp::LspContext;
    // https://github.com/rust-lang/rust-analyzer/blob/master/lib/lsp-server/examples/goto_def.rs

    pub fn start() -> Result<(), Box<dyn Error>> {
        log_file("Started");
        eprint!("Starting LSP server\n");
        let (connection, io_threads) = Connection::stdio();

        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            // diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            //     work_done_progress_options: Default::default(),
            //     identifier: None,
            //     workspace_diagnostics: false,
            //     inter_file_dependencies: false,
            // })),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
            completion_provider: None,
            diagnostic_provider: None,
            declaration_provider: None,
            implementation_provider: None,
            type_definition_provider: None,
            document_highlight_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            ..Default::default()
        })
        .unwrap();

        let initialization_params = match connection.initialize(server_capabilities) {
            Ok(it) => it,
            Err(e) => {
                if e.channel_is_disconnected() {
                    io_threads.join()?;
                }
                return Err(e.into());
            }
        };
        handle_conn(connection, initialization_params).unwrap();
        io_threads.join()?;

        eprint!("LSP server stopped\n");
        Ok(())
    }

    fn log_file(msg: &str) {
        use std::fs::OpenOptions;
        use std::io::Write;
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open("lsp.log")
            .unwrap();
        writeln!(file, "{}", msg).unwrap();
    }

    pub fn handle_conn(
        connection: Connection,
        params: Value,
    ) -> Result<(), Box<dyn Error + Sync + Send>> {
        let _params: InitializeParams = serde_json::from_value(params).unwrap();
        let mut docs: HashMap<String, String> = HashMap::new();
        for msg in &connection.receiver {
            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    log_file(&format!("{req:?}"));

                    match cast_req::<HoverRequest>(req) {
                        Ok((id, hov_req)) => {
                            let doc = docs
                                .get(
                                    &hov_req
                                        .text_document_position_params
                                        .text_document
                                        .uri
                                        .to_string(),
                                )
                                .unwrap();
                            let lsp_ctx = parse_ebnf(doc)
                                .unwrap()
                                .1
                                .lsp_context
                                .hover(crate::lsp::lsp::Location {
                                    line: hov_req.text_document_position_params.position.line
                                        as usize,
                                    col: hov_req.text_document_position_params.position.character
                                        as usize,
                                })
                                .unwrap();
                            let result = Some(Hover {
                                range: None,
                                contents: lsp_types::HoverContents::Scalar(
                                    lsp_types::MarkedString::String(lsp_ctx.to_string()),
                                ),
                            });
                            let json_result = serde_json::to_value(result).unwrap();
                            let resp = Response {
                                id,
                                result: Some(json_result),
                                error: None,
                            };

                            connection.sender.send(Message::Response(resp)).unwrap();
                        }
                        Err(_) => todo!(),
                    }
                }
                Message::Response(resp) => {
                    log_file(&format!("{resp:?}"));

                    eprintln!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    log_file(&format!("{not:?}"));

                    match not.method.as_str() {
                        notification::DidOpenTextDocument::METHOD => {
                            let params: DidOpenTextDocumentParams =
                                not.extract(DidOpenTextDocument::METHOD).unwrap();
                            log_file(&format!("{params:?}"));
                            docs.insert(
                                params.text_document.uri.to_string(),
                                params.text_document.text.to_string(),
                            );
                        }
                        notification::DidChangeTextDocument::METHOD => {
                            let params: lsp_types::DidChangeTextDocumentParams = not
                                .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                                .unwrap();
                            log_file(&format!("{params:?}"));
                        }
                        _ => {}
                    }

                    // eprintln!("got notification: {not:?}");
                }
            }
        }
        Ok(())
    }

    fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        req.extract(R::METHOD)
    }
}
