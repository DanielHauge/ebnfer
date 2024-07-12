pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::collections::HashMap;
    use std::error::Error;

    use lsp_types::notification::{self, DidOpenTextDocument, Notification};
    use lsp_types::request::{HoverRequest, Request};
    use lsp_types::{
        DidOpenTextDocumentParams, Hover, HoverParams, HoverProviderCapability, LanguageString,
        MarkedString, OneOf, Position, TextDocumentSyncCapability, TextDocumentSyncKind,
    };
    use lsp_types::{InitializeParams, ServerCapabilities};

    use lsp_server::{
        Connection, ExtractError, Message, Request, RequestId, Response, ResponseError,
    };
    use serde_json::Value;

    use crate::lsp::lsp::{Location, LspContext};
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
        let mut lsp_context: HashMap<String, Result<LspContext, String>> = HashMap::new();
        for msg in &connection.receiver {
            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    log_file(&format!("{req:?}"));

                    match req.method.as_str() {
                        lsp_types::request::HoverRequest::METHOD => {
                            let (id, param) =
                                req.extract(HoverRequest::METHOD).expect("Cast failed");
                            match hover(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::References::METHOD => {
                            let (id, param): (RequestId, lsp_types::ReferenceParams) = req
                                .extract(lsp_types::request::References::METHOD)
                                .expect("Failed to cast");
                        }
                        _ => {}
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
                            match parse_ebnf(&params.text_document.text) {
                                Ok(ctx) => {
                                    lsp_context
                                        .insert(params.text_document.uri.to_string(), Ok(ctx));
                                }
                                Err(e) => {
                                    lsp_context
                                        .insert(params.text_document.uri.to_string(), Err(e));
                                }
                            }
                            // Send diagnostics errors?
                        }
                        notification::DidChangeTextDocument::METHOD => {
                            let params: lsp_types::DidChangeTextDocumentParams = not
                                .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                                .unwrap();
                            log_file(&format!("{params:?}"));
                            match parse_ebnf(&params.content_changes[0].text) {
                                Ok(ctx) => {
                                    lsp_context
                                        .insert(params.text_document.uri.to_string(), Ok(ctx));
                                }
                                Err(e) => {
                                    lsp_context
                                        .insert(params.text_document.uri.to_string(), Err(e));
                                }
                            }
                            // Send diagnostics errors?
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_ebnf(doc: &str) -> Result<LspContext, String> {
        let lexer = ebnf_parser::Lexer::new(doc);
        let parser = ebnf_parser::Parser::new(lexer);
        let parse_results = parser.parse();
        match parse_results {
            Ok(x) => Ok(LspContext::from_parse_results(doc, &x)),
            Err(e) => Err(e.to_string()),
        }
    }

    fn error(msg: &str, id: RequestId) -> Message {
        Message::Response(Response {
            id,
            result: None,
            error: Some(ResponseError {
                code: 1,
                message: msg.to_string(),
                data: None,
            }),
        })
    }

    impl From<Position> for Location {
        fn from(pos: Position) -> Self {
            Location {
                line: pos.line as usize,
                col: pos.character as usize,
            }
        }
    }

    fn hover(
        lsp_context: &HashMap<String, Result<LspContext, String>>,
        id: RequestId,
        params: HoverParams,
    ) -> Result<Message, String> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();

        match lsp_context.get(&uri) {
            Some(Ok(ctx)) => {
                let hover = match ctx.hover(&Location::from(
                    params.text_document_position_params.position,
                )) {
                    Some(x) => x.trim().to_string(),
                    None => {
                        return Ok(Message::Response(Response {
                            id,
                            result: None,
                            error: None,
                        }))
                    }
                };
                let marked_string = MarkedString::LanguageString(LanguageString {
                    language: "ebnf".to_string(),
                    value: hover.trim().to_string(),
                });
                let result = Some(Hover {
                    range: None,
                    contents: lsp_types::HoverContents::Scalar(marked_string),
                });
                let json_result = serde_json::to_value(result).expect("Failed to serialize");
                Ok(Message::Response(Response {
                    id,
                    result: Some(json_result),
                    error: None,
                }))
            }
            Some(Err(e)) => Err(e.to_string()),
            None => Err("No document found".to_string()),
        }
    }
}
