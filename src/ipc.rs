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
    use lsp_types::{CompletionItem, Documentation, PrepareRenameResponse, WorkspaceEdit};
    use lsp_types::{
        Diagnostic, DiagnosticOptions, DiagnosticSeverity, DiagnosticTag, DocumentDiagnosticParams,
        FullDocumentDiagnosticReport, ReferenceParams, SemanticToken, SemanticTokenModifier,
        SemanticTokenType, SemanticTokensLegend, SemanticTokensParams, SemanticTokensResult, Uri,
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
            rename_provider: Some(OneOf::Right(lsp_types::RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            completion_provider: Some(lsp_types::CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: None,
                all_commit_characters: None,
                work_done_progress_options: Default::default(),
                completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
            }),
            declaration_provider: None, // No declarations, rules are always defined
            implementation_provider: None,
            type_definition_provider: None, // No types, only production rules
            // document_highlight_provider: Some(OneOf::Left(true)),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            code_action_provider: None,
            document_symbol_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                    lsp_types::SemanticTokensOptions {
                        work_done_progress_options: Default::default(),
                        legend: SemanticTokensLegend {
                            token_types: vec![SemanticTokenType::ENUM_MEMBER],
                            token_modifiers: vec![SemanticTokenModifier::STATIC],
                        },
                        range: None,
                        full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                    },
                ),
            ),
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
        let mut parser_error: HashMap<String, Option<LspError>> = HashMap::new();
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
                            match diagnostics(&lsp_context, &parser_error, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::SemanticTokensFullRequest::METHOD => {
                            let (id, param): (RequestId, lsp_types::SemanticTokensParams) = req
                                .extract(lsp_types::request::SemanticTokensFullRequest::METHOD)
                                .expect("Failed to cast");
                            match semantic_tokens(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::Completion::METHOD => {
                            let (id, param): (RequestId, lsp_types::CompletionParams) = req
                                .extract(lsp_types::request::Completion::METHOD)
                                .expect("Failed to cast");
                            match completion(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::Formatting::METHOD => {
                            let (id, param): (RequestId, lsp_types::DocumentFormattingParams) = req
                                .extract(lsp_types::request::Formatting::METHOD)
                                .expect("Failed to cast");
                            match format(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::Rename::METHOD => {
                            let (id, param): (RequestId, lsp_types::RenameParams) = req
                                .extract(lsp_types::request::Rename::METHOD)
                                .expect("Failed to cast");
                            match rename(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::PrepareRenameRequest::METHOD => {
                            let (id, param): (RequestId, lsp_types::TextDocumentPositionParams) =
                                req.extract(lsp_types::request::PrepareRenameRequest::METHOD)
                                    .expect("Failed to cast");
                            match rename_prepare(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
                        }
                        lsp_types::request::GotoDefinition::METHOD => {
                            let (id, param): (RequestId, lsp_types::GotoDefinitionParams) = req
                                .extract(lsp_types::request::GotoDefinition::METHOD)
                                .expect("Failed to cast");
                            match goto_definition(&lsp_context, id.clone(), param) {
                                Ok(x) => connection.sender.send(x).unwrap(),
                                Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                            }
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
                            let ctx = LspContext::from_src(params.text_document.text);
                            let p_error = ctx.syntax_error_to_lsp_error();
                            if let Some(x) = p_error {
                                parser_error
                                    .insert(params.text_document.uri.to_string(), Some(x.clone()));
                            } else {
                                lsp_context.insert(params.text_document.uri.to_string(), ctx);
                                parser_error.insert(params.text_document.uri.to_string(), None);
                            }
                        }
                        notification::DidChangeTextDocument::METHOD => {
                            let mut params: lsp_types::DidChangeTextDocumentParams = not
                                .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                                .unwrap();
                            log_file(&format!("{params:?}"));
                            let first_change = params.content_changes.remove(0);
                            let ctx = LspContext::from_src(first_change.text);
                            let p_error = ctx.syntax_error_to_lsp_error();
                            if let Some(x) = p_error {
                                parser_error
                                    .insert(params.text_document.uri.to_string(), Some(x.clone()));
                            } else {
                                lsp_context.insert(params.text_document.uri.to_string(), ctx);
                                parser_error.insert(params.text_document.uri.to_string(), None);
                            }
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

    fn goto_definition(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: lsp_types::GotoDefinitionParams,
    ) -> Result<Message, String> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let loc = crate::lsp::lsp::Location::from(params.text_document_position_params.position);
        let def_length = ctx.symbol(&loc).ok_or("No symbol found")?.len();
        let def_loc = ctx.definition(&loc).ok_or("No definition found")?;
        let resp = lsp_types::GotoDefinitionResponse::Array(vec![
            lsp_types::Location::from_with_uri_length(
                def_loc.clone(),
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .clone(),
                def_length,
            ),
        ]);
        let json_result = serde_json::to_value(resp).expect("Failed to serialize");
        log_file(&format!("{json_result:?}"));
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn semantic_tokens(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: SemanticTokensParams,
    ) -> Result<Message, String> {
        let uri = params.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let tokens = ctx.root_rule().ok_or("No root rule found")?;
        let result = SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
            result_id: None,
            data: vec![SemanticToken {
                delta_line: tokens.0.line as u32,
                delta_start: tokens.0.col as u32,
                length: (tokens.1.col - tokens.0.col) as u32,
                token_type: 0,
                token_modifiers_bitset: 0,
            }],
        });
        let json_result = serde_json::to_value(result).expect("Failed to serialize");
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn rename_prepare(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: lsp_types::TextDocumentPositionParams,
    ) -> Result<Message, String> {
        let uri = params.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let loc = crate::lsp::lsp::Location::from(params.position);
        let result = match ctx.symbol(&loc) {
            Some(_) => {
                let resp = PrepareRenameResponse::DefaultBehavior {
                    default_behavior: true,
                };
                let json_result = serde_json::to_value(resp).expect("Failed to serialize");
                Some(json_result)
            }
            None => None,
        };
        Ok(Message::Response(Response {
            id,
            result,
            error: None,
        }))
    }

    fn rename(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: lsp_types::RenameParams,
    ) -> Result<Message, String> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let loc = crate::lsp::lsp::Location::from(params.text_document_position.position);
        let symbol_len = ctx.symbol(&loc).ok_or("No symbol found")?.len();
        let main_def = ctx.definition(&loc).ok_or("No definition found")?;
        let alt_defs = ctx.alternative_definitions(&loc);
        let refs = ctx.references(&loc).map_or(vec![], |x| x);
        let all_locs = alt_defs
            .iter()
            .chain(std::iter::once(main_def))
            .chain(refs.iter());
        let new_name = params.new_name;
        let text_edits: Vec<lsp_types::TextEdit> = all_locs
            .map(|x| lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: x.line as u32,
                        character: x.col as u32,
                    },
                    end: lsp_types::Position {
                        line: x.line as u32,
                        character: (x.col + symbol_len) as u32,
                    },
                },
                new_text: new_name.clone(),
            })
            .collect();

        let mut edits: HashMap<Uri, Vec<lsp_types::TextEdit>> = HashMap::new();
        edits.insert(
            params.text_document_position.text_document.uri.clone(),
            text_edits,
        );
        let resp = WorkspaceEdit {
            changes: Some(edits),
            document_changes: None,
            change_annotations: None,
        };
        let json_result = serde_json::to_value(resp).expect("Failed to serialize");
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn format(
        lsp_context: &HashMap<String, LspContext>,
        id: RequestId,
        params: lsp_types::DocumentFormattingParams,
    ) -> Result<Message, String> {
        let ctx = lsp_context
            .get(&params.text_document.uri.to_string())
            .ok_or("No document found")?;
        let formatted = ctx.format();

        match formatted {
            None => Ok(Message::Response(Response {
                id,
                result: None,
                error: Some(ResponseError {
                    code: 1,
                    message: "Failed to format".to_string(),
                    data: None,
                }),
            })),
            Some(x) => {
                let resp: Vec<lsp_types::TextEdit> = vec![lsp_types::TextEdit {
                    range: lsp_types::Range {
                        start: lsp_types::Position {
                            line: 0,
                            character: 0,
                        },
                        end: lsp_types::Position {
                            line: 100000, // Who'd f*ck will ever format EBNF bigger than this?
                            character: 100000,
                        },
                    },
                    new_text: x,
                }];
                log_file(&format!("{resp:?}"));
                let json_result = serde_json::to_value(resp).expect("Failed to serialize");
                Ok(Message::Response(Response {
                    id,
                    result: Some(json_result),
                    error: None,
                }))
            }
        }
    }

    fn completion(
        lsp_context: &HashMap<String, LspContext>,
        id: lsp_server::RequestId,
        params: lsp_types::CompletionParams,
    ) -> Result<Message, String> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;

        let symbols = ctx
            .symbols()
            .into_iter()
            .map(|x| {
                let hover = ctx.hover_from_def(&x);
                let mut item = CompletionItem::new_simple(x, "stuff".to_string());
                if let Some(h) = hover {
                    item.documentation = Some(Documentation::String(h.to_string()));
                    // let detail = h.split("=").take(1).collect();
                    let description = h.split("=").skip(1).take(1).collect();
                    item.label_details = Some(lsp_types::CompletionItemLabelDetails {
                        detail: None,
                        description: Some(description),
                    });
                }
                item.detail = Some("What".to_string());

                item.kind = Some(lsp_types::CompletionItemKind::VARIABLE);

                item
            })
            .collect::<Vec<_>>();
        let resp = lsp_types::CompletionResponse::Array(symbols);
        let json_result = serde_json::to_value(resp).expect("Failed to serialize");
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
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
        let mut ref_response: Vec<lsp_types::Location> = refs
            .iter()
            .map(|x| {
                lsp_types::Location::from_with_uri_length(
                    x.clone(),
                    params.text_document_position.text_document.uri.clone(),
                    defs_len,
                )
            })
            .collect();
        if params.context.include_declaration {
            let mut alt_defs = ctx.alternative_definitions(&loc);
            alt_defs.push(ctx.definition(&loc).ok_or("No definition found")?.clone());
            let lsp_locs = alt_defs
                .iter()
                .map(|x| {
                    lsp_types::Location::from_with_uri_length(
                        x.clone(),
                        params.text_document_position.text_document.uri.clone(),
                        defs_len,
                    )
                })
                .collect::<Vec<_>>();
            ref_response.extend(lsp_locs);
        }
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
        parser_error: &HashMap<String, Option<LspError>>,
        id: RequestId,
        params: DocumentDiagnosticParams,
    ) -> Result<Message, String> {
        let p_error = parser_error
            .get(params.text_document.uri.as_str())
            .ok_or("document not found")?;
        let e: Vec<Diagnostic> = match p_error {
            None => vec![],
            Some(x) => vec![x.clone().into()],
        };

        let uri = params.text_document.uri.to_string();
        let ctx = lsp_context.get(&uri).ok_or("No document found")?;
        let items = ctx
            .diagnostics()
            .into_iter()
            .map(|x| x.into())
            .chain(e)
            .collect();
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
        let main_hover_str = MarkedString::LanguageString(LanguageString {
            language: "ebnf".to_string(),
            value: hover.trim().to_string(),
        });

        let mut alt_hovers_str = ctx
            .hover_alternatives(&Location::from(
                params.text_document_position_params.position,
            ))
            .into_iter()
            .map(|x| {
                MarkedString::LanguageString(LanguageString {
                    language: "ebnf".to_string(),
                    value: x.trim().to_string(),
                })
            })
            .collect::<Vec<MarkedString>>();

        let resp = match alt_hovers_str.len() {
            0 => Hover {
                range: None,
                contents: lsp_types::HoverContents::Scalar(main_hover_str),
            },
            _ => {
                alt_hovers_str.push(main_hover_str);
                Hover {
                    range: None,
                    contents: lsp_types::HoverContents::Array(alt_hovers_str).into(),
                }
            }
        };

        let result = Some(resp);
        let json_result = serde_json::to_value(result).expect("Failed to serialize");
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }
}
