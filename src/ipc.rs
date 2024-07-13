pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::collections::HashMap;
    use std::error::Error;

    use lsp_types::notification::{self, DidOpenTextDocument, Notification};
    use lsp_types::request::HoverRequest;
    use lsp_types::{
        request::Request, DiagnosticServerCapabilities, DidOpenTextDocumentParams, Hover,
        HoverParams, HoverProviderCapability, LanguageString, MarkedString, OneOf, Position,
        TextDocumentSyncCapability, TextDocumentSyncKind,
    };
    use lsp_types::{
        Diagnostic, DiagnosticOptions, DiagnosticSeverity, DiagnosticTag, DocumentDiagnosticParams,
        FullDocumentDiagnosticReport, ReferenceParams, Uri,
    };
    use lsp_types::{InitializeParams, ServerCapabilities};

    use lsp_server::{Connection, Message, RequestId, Response, ResponseError};
    use serde_json::Value;

    use crate::lsp::lsp::{Location, LspContext, LspError};
    // https://github.com/rust-lang/rust-analyzer/blob/master/lib/lsp-server/examples/goto_def.rs

    pub fn start() -> Result<(), Box<dyn Error>> {
        log_file("Started");
        eprint!("Starting LSP server\n");
        let (connection, io_threads) = Connection::stdio();

        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                work_done_progress_options: Default::default(),
                identifier: None,
                workspace_diagnostics: false,
                inter_file_dependencies: false,
            })),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
            completion_provider: None,
            // diagnostic_provider: None,
            declaration_provider: None,
            implementation_provider: None,
            type_definition_provider: None,
            document_highlight_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            code_action_provider: None,
            document_symbol_provider: None,
            semantic_tokens_provider: None,
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
        let mut lsp_context: HashMap<String, LspContext> = HashMap::new();
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
                            match references(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::DocumentDiagnosticRequest::METHOD => {
                            let (id, param): (RequestId, lsp_types::DocumentDiagnosticParams) = req
                                .extract(lsp_types::request::DocumentDiagnosticRequest::METHOD)
                                .expect("Failed to cast");
                            match diagnostics(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }

                            // let context = lsp_context.get(_param.text_document.uri.as_str())
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
                            lsp_context.insert(
                                params.text_document.uri.to_string(),
                                LspContext::from_src(&params.text_document.text),
                            );
                        }
                        notification::DidChangeTextDocument::METHOD => {
                            let params: lsp_types::DidChangeTextDocumentParams = not
                                .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                                .unwrap();
                            log_file(&format!("{params:?}"));
                            lsp_context.insert(
                                params.text_document.uri.to_string(),
                                LspContext::from_src(&params.content_changes[0].text),
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
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

    trait FromWithUriLength<T> {
        fn from_with_uri_length(t: T, uri: Uri, length: usize) -> Self;
    }

    impl FromWithUriLength<Location> for lsp_types::Location {
        fn from_with_uri_length(loc: Location, t: Uri, length: usize) -> Self {
            lsp_types::Location {
                uri: t,
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: loc.line as u32,
                        character: loc.col as u32,
                    },
                    end: lsp_types::Position {
                        line: loc.line as u32,
                        character: (loc.col + length) as u32,
                    },
                },
            }
        }
    }

    impl Into<Diagnostic> for LspError {
        fn into(self) -> Diagnostic {
            let severity = match self.error_type {
                crate::lsp::lsp::LspErrorType::SyntaxError => DiagnosticSeverity::ERROR,
                crate::lsp::lsp::LspErrorType::UnusedDefinition => DiagnosticSeverity::WARNING,
                crate::lsp::lsp::LspErrorType::UndefinedReference => DiagnosticSeverity::ERROR,
            };
            let tags = match self.error_type {
                crate::lsp::lsp::LspErrorType::UnusedDefinition => {
                    Some(vec![DiagnosticTag::UNNECESSARY])
                }
                _ => None,
            };
            Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: self.start.line as u32,
                        character: self.start.col as u32,
                    },
                    end: lsp_types::Position {
                        line: self.end.line as u32,
                        character: self.end.col as u32,
                    },
                },
                severity: Some(severity),
                code: None,
                source: None,
                message: self.message,
                related_information: None,
                tags,
                code_description: None,
                data: None,
            }
        }
    }

    fn references(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: ReferenceParams,
    ) -> Result<Message, String> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let loc = crate::lsp::lsp::Location::from(params.text_document_position.position);
        let refs = ctx.references(&loc).ok_or("No references found")?;
        let symbol = ctx.symbol(&loc).ok_or("No symbol found")?;
        let defs_len = symbol.len();
        let ref_response: Vec<lsp_types::Location> = refs
            .iter()
            .map(|x| {
                lsp_types::Location::from_with_uri_length(
                    x.clone(),
                    params.text_document_position.text_document.uri.clone(),
                    defs_len,
                )
            })
            .collect();
        let json_result = serde_json::to_value(ref_response)
            .ok()
            .ok_or("Failed to serialize")?;
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn diagnostics(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: DocumentDiagnosticParams,
    ) -> Result<Message, String> {
        let uri = params.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let items = ctx.diagnostics().into_iter().map(|x| x.into()).collect();
        let report = FullDocumentDiagnosticReport {
            items,
            result_id: None,
        };
        let json_result = serde_json::to_value(report).expect("Failed to serialize");
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn hover(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: HoverParams,
    ) -> Result<Message, String> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();

        let ctx = lsp_context.get(&uri).ok_or("No document found")?;

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
}
