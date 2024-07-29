#![allow(clippy::print_stderr)]

use std::{collections::HashMap, error::Error};

use lsp_types::notification::{self, DidOpenTextDocument, Notification};
use lsp_types::request::{
    CodeActionRequest, Completion, DocumentDiagnosticRequest, DocumentSymbolRequest, Formatting,
    GotoDefinition, HoverRequest, PrepareRenameRequest, References, Rename,
    SemanticTokensFullRequest,
};
use lsp_types::{
    request::Request, DiagnosticServerCapabilities, DidOpenTextDocumentParams, Hover, HoverParams,
    HoverProviderCapability, LanguageString, MarkedString, OneOf, Position,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};
use lsp_types::{
    CodeAction, CodeActionProviderCapability, CodeActionResponse, CompletionItem, DocumentSymbol,
    Documentation, PrepareRenameResponse, Range, SymbolKind, WorkspaceEdit,
};
use lsp_types::{
    Diagnostic, DiagnosticOptions, DiagnosticSeverity, DiagnosticTag, DocumentDiagnosticParams,
    FullDocumentDiagnosticReport, ReferenceParams, SemanticToken, SemanticTokenModifier,
    SemanticTokenType, SemanticTokensLegend, SemanticTokensResult, Uri,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{Connection, Message, RequestId, Response, ResponseError};
use serde_json::Value;

use crate::lsp::{AnalysisContext, SUPRESS_UNUSED_DEF, UNUSED_DEF_FMT};
use crate::lsp::{Location, LspError};
// https://github.com/rust-lang/rust-analyzer/blob/master/lib/lsp-server/examples/goto_def.rs

pub fn start() -> Result<(), Box<dyn Error>> {
    log_file("Started");
    eprintln!("Starting LSP server");
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
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
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
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

    eprintln!("LSP server stopped");
    Ok(())
}

#[cfg(debug_assertions)]
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

#[cfg(not(debug_assertions))]
fn log_file(_msg: &str) {}

// type alias: ctx = (HashMap<String, LspContext>, HashMap<String, Option<LspError>>)
struct LspContext {
    lsp: HashMap<String, AnalysisContext>,
    error: HashMap<String, Option<LspError>>,
}

pub fn handle_conn(
    connection: Connection,
    params: Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).or(Err("Failed to parse"))?;
    let mut lsp_context = LspContext {
        lsp: HashMap::new(),
        error: HashMap::new(),
    };
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                log_file(&format!("{req:?}"));

                let req_id = req.id.clone();

                let handle =
                    |rqs, func: fn(ctx: &LspContext, msg: Message) -> Result<Message, String>| {
                        match func(&lsp_context, Message::Request(rqs)) {
                            Ok(x) => connection.sender.send(x).or(Err("Failed to send")),
                            Err(e) => connection
                                .sender
                                .send(error(&e, req_id))
                                .or(Err("Failed to send")),
                        }
                    };

                match req.method.as_str() {
                    HoverRequest::METHOD => handle(req, hover)?,
                    References::METHOD => handle(req, references)?,
                    DocumentDiagnosticRequest::METHOD => handle(req, diagnostics)?,
                    SemanticTokensFullRequest::METHOD => handle(req, semantic_tokens)?,
                    Completion::METHOD => handle(req, completion)?,
                    Formatting::METHOD => handle(req, format)?,
                    Rename::METHOD => handle(req, rename)?,
                    PrepareRenameRequest::METHOD => handle(req, rename_prepare)?,
                    GotoDefinition::METHOD => handle(req, goto_definition)?,
                    DocumentSymbolRequest::METHOD => handle(req, symbols)?,
                    CodeActionRequest::METHOD => handle(req, code_actions)?,
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
                        let ctx = AnalysisContext::from_src(params.text_document.text);
                        let p_error = ctx.syntax_error_to_lsp_error();
                        if let Some(x) = p_error {
                            lsp_context
                                .error
                                .insert(params.text_document.uri.to_string(), Some(x.clone()));
                        } else {
                            lsp_context
                                .lsp
                                .insert(params.text_document.uri.to_string(), ctx);
                            lsp_context
                                .error
                                .insert(params.text_document.uri.to_string(), None);
                        }
                    }
                    notification::DidChangeTextDocument::METHOD => {
                        let mut params: lsp_types::DidChangeTextDocumentParams = not
                            .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                            .unwrap();
                        log_file(&format!("{params:?}"));
                        let first_change = params.content_changes.remove(0);
                        let ctx = AnalysisContext::from_src(first_change.text);
                        let p_error = ctx.syntax_error_to_lsp_error();
                        if let Some(x) = p_error {
                            lsp_context
                                .error
                                .insert(params.text_document.uri.to_string(), Some(x.clone()));
                        } else {
                            lsp_context
                                .lsp
                                .insert(params.text_document.uri.to_string(), ctx);
                            lsp_context
                                .error
                                .insert(params.text_document.uri.to_string(), None);
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

impl From<LspError> for Diagnostic {
    fn from(val: LspError) -> Self {
        let severity = match val.error_type {
            crate::lsp::LspErrorType::SyntaxError => DiagnosticSeverity::ERROR,
            crate::lsp::LspErrorType::UnusedDefinition => DiagnosticSeverity::WARNING,
            crate::lsp::LspErrorType::UndefinedReference => DiagnosticSeverity::ERROR,
        };
        let tags = match val.error_type {
            crate::lsp::LspErrorType::UnusedDefinition => Some(vec![DiagnosticTag::UNNECESSARY]),
            _ => None,
        };
        Diagnostic {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: val.start.line as u32,
                    character: val.start.col as u32,
                },
                end: lsp_types::Position {
                    line: val.end.line as u32,
                    character: val.end.col as u32,
                },
            },
            severity: Some(severity),
            code: None,
            source: None,
            message: val.message,
            related_information: None,
            tags,
            code_description: None,
            data: None,
        }
    }
}

fn goto_definition(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::GotoDefinitionParams) =
        extract_req(msg, GotoDefinition::METHOD);
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
    let loc = crate::lsp::Location::from(params.text_document_position_params.position);
    let def_length = ctx.symbol(&loc).ok_or("No symbol found")?.len();
    let def_loc = ctx.definition(&loc).ok_or("No definition found")?;
    let resp =
        lsp_types::GotoDefinitionResponse::Array(vec![lsp_types::Location::from_with_uri_length(
            def_loc.clone(),
            params
                .text_document_position_params
                .text_document
                .uri
                .clone(),
            def_length,
        )]);
    let json_result = serde_json::to_value(resp).expect("Failed to serialize");
    log_file(&format!("{json_result:?}"));
    Ok(Message::Response(Response {
        id,
        result: Some(json_result),
        error: None,
    }))
}

fn symbols(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::DocumentSymbolParams) =
        extract_req(msg, DocumentSymbolRequest::METHOD);
    let uri = params.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
    let symbols = ctx.symbols();
    let symbol_infos: Vec<DocumentSymbol> = symbols
        .into_iter()
        .map(|x| {
            #[allow(deprecated)] //Not using deprecated fields, will use tags if needed
            DocumentSymbol {
                detail: None,
                kind: SymbolKind::FUNCTION,
                deprecated: None, //Deprecated, use tags
                tags: None,
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: x.1.line as u32,
                        character: x.1.col as u32,
                    },
                    end: lsp_types::Position {
                        line: x.1.line as u32,
                        character: x.1.col as u32 + x.0.len() as u32,
                    },
                },
                selection_range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: x.1.line as u32,
                        character: x.1.col as u32,
                    },
                    end: lsp_types::Position {
                        line: x.2.line as u32,
                        character: x.2.col as u32,
                    },
                },
                children: Some(
                    ctx.alternative_definitions(&x.1)
                        .into_iter()
                        .flat_map(|y| {
                            let alternative_hovers = ctx.hover_alternatives(&y);
                            let doc_symbols: Vec<DocumentSymbol> = alternative_hovers
                                .into_iter()
                                .map(|h| {
                                    let hover_len = h.len();
                                    DocumentSymbol {
                                        deprecated: None, //Deprecated, use tags
                                        detail: None,
                                        kind: SymbolKind::FUNCTION,
                                        name: x.0.clone(),
                                        children: None,
                                        tags: None,
                                        range: lsp_types::Range {
                                            start: lsp_types::Position {
                                                line: y.line as u32,
                                                character: y.col as u32,
                                            },
                                            end: lsp_types::Position {
                                                line: y.line as u32,
                                                character: y.col as u32 + x.0.len() as u32,
                                            },
                                        },
                                        selection_range: lsp_types::Range {
                                            start: lsp_types::Position {
                                                line: y.line as u32,
                                                character: y.col as u32,
                                            },
                                            end: lsp_types::Position {
                                                line: y.line as u32,
                                                character: y.col as u32 + hover_len as u32,
                                            },
                                        },
                                    }
                                })
                                .collect();
                            doc_symbols
                        })
                        .collect(),
                ),
                name: x.0,
            }
        })
        .collect();

    let resp = lsp_types::DocumentSymbolResponse::Nested(symbol_infos);
    let json_result = serde_json::to_value(resp).expect("Failed to serialize");
    Ok(Message::Response(Response {
        id,
        result: Some(json_result),
        error: None,
    }))
}

fn code_actions(_lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::CodeActionParams) =
        extract_req(msg, lsp_types::request::CodeActionRequest::METHOD);

    let mut result: CodeActionResponse = vec![];

    let unused_def_diag = params
        .context
        .diagnostics
        .into_iter()
        .find(|x| x.message.starts_with(UNUSED_DEF_FMT));

    if let Some(x) = unused_def_diag {
        let loc = Location::from(x.range.start);
        let mut changes: HashMap<Uri, Vec<lsp_types::TextEdit>> = HashMap::new();
        let p = Position {
            line: loc.line as u32,
            character: loc.col as u32,
        };
        let range = Range { start: p, end: p };
        changes.insert(
            params.text_document.uri.clone(),
            vec![lsp_types::TextEdit {
                range: range,
                new_text: format!("(* {} *)\n", SUPRESS_UNUSED_DEF),
            }],
        );
        let add_supress_rule_action = CodeAction {
            data: None,
            title: format!("Suppress rule → {}", x.message),
            diagnostics: Some(vec![x]),
            kind: Some(lsp_types::CodeActionKind::QUICKFIX),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: None,
            disabled: None,
        };
        result.push(lsp_types::CodeActionOrCommand::CodeAction(
            add_supress_rule_action,
        ));
    }

    let json_result = serde_json::to_value(result).expect("Failed to serialize");
    Ok(Message::Response(Response {
        id,
        result: Some(json_result),
        error: None,
    }))
}

fn semantic_tokens(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::SemanticTokensParams) =
        extract_req(msg, SemanticTokensFullRequest::METHOD);
    let uri = params.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
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

fn rename_prepare(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::TextDocumentPositionParams) =
        extract_req(msg, PrepareRenameRequest::METHOD);
    let uri = params.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
    let loc = crate::lsp::Location::from(params.position);
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

fn rename(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::RenameParams) = extract_req(msg, Rename::METHOD);
    let uri = params.text_document_position.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
    let loc = crate::lsp::Location::from(params.text_document_position.position);
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

    #[allow(clippy::mutable_key_type)]
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

fn format(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::DocumentFormattingParams) =
        extract_req(msg, Formatting::METHOD);
    let ctx = lsp_context
        .lsp
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

fn completion(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::CompletionParams) =
        extract_req(msg, Completion::METHOD);
    let uri = params.text_document_position.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;

    let symbols = ctx
        .symbols()
        .into_iter()
        .map(|x| {
            let hover = ctx.hover_from_def(&x.0);
            let mut item = CompletionItem::new_simple(x.0, "stuff".to_string());
            if let Some(h) = hover {
                item.documentation = Some(Documentation::String(h.to_string()));
                let description = h.split('=').skip(1).take(1).collect();
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

fn references(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, ReferenceParams) = extract_req(msg, References::METHOD);
    let uri = params.text_document_position.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
    let loc = crate::lsp::Location::from(params.text_document_position.position);
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

fn diagnostics(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, DocumentDiagnosticParams) =
        extract_req(msg, DocumentDiagnosticRequest::METHOD);
    let p_error = lsp_context
        .error
        .get(params.text_document.uri.as_str())
        .ok_or("document not found")?;
    let e: Vec<Diagnostic> = match p_error {
        None => vec![],
        Some(x) => vec![x.clone().into()],
    };

    let uri = params.text_document.uri.to_string();
    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;
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

fn hover(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, HoverParams) = extract_req(msg, HoverRequest::METHOD);
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();

    let ctx = lsp_context.lsp.get(&uri).ok_or("No document found")?;

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
                contents: lsp_types::HoverContents::Array(alt_hovers_str),
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

fn extract_req<T>(msg: Message, method: &str) -> (RequestId, T)
where
    T: serde::de::DeserializeOwned,
{
    let (id, params) = try_extract_req(msg, method).expect("Failed to cast");
    (id, params)
}

fn try_extract_req<T>(msg: Message, method: &str) -> Option<(RequestId, T)>
where
    T: serde::de::DeserializeOwned,
{
    match msg {
        Message::Request(req) => {
            if req.method == method {
                let (id, params) = req.extract(method).expect("Failed to cast");
                Some((id, params))
            } else {
                None
            }
        }
        _ => None,
    }
}
