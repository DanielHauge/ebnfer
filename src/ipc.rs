#![allow(clippy::print_stderr)]

use std::{collections::HashMap, error::Error, path::Path};

use lsp_types::notification::{
    self, DidChangeWatchedFiles, DidChangeWorkspaceFolders, DidCloseTextDocument,
    DidOpenTextDocument, Initialized, Notification,
};
use lsp_types::request::{
    CodeActionRequest, Completion, DocumentDiagnosticRequest, DocumentSymbolRequest, Formatting,
    GotoDefinition, HoverRequest, PrepareRenameRequest, References, RegisterCapability, Rename,
    SemanticTokensFullRequest, WorkspaceDiagnosticRefresh,
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
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, FileSystemWatcher,
    FullDocumentDiagnosticReport, GlobPattern, ReferenceParams, Registration, RegistrationParams,
    RelatedFullDocumentDiagnosticReport, SemanticToken, SemanticTokenModifier, SemanticTokenType,
    SemanticTokensLegend, SemanticTokensResult, Uri, WorkspaceFoldersServerCapabilities,
    WorkspaceServerCapabilities,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{
    Connection, Message, Request as ServerRequest, RequestId, Response, ResponseError,
};
use serde_json::Value;

use crate::lsp::{Location, LspError};
use crate::lsp::{SUPRESS_UNUSED_DEF, UNUSED_DEF_FMT};
use crate::workspace::{path_to_uri, WorkspaceState};
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
            inter_file_dependencies: true,
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
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: None,
        }),
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

struct LspContext {
    workspace: WorkspaceState,
    register_file_watcher: bool,
    refresh_diagnostics: bool,
    next_request_id: u32,
}

pub fn handle_conn(
    connection: Connection,
    params: Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params: InitializeParams = serde_json::from_value(params).or(Err("Failed to parse"))?;
    let register_file_watcher = params
        .capabilities
        .workspace
        .as_ref()
        .and_then(|workspace| workspace.did_change_watched_files)
        .and_then(|watched_files| watched_files.dynamic_registration)
        .unwrap_or(false);
    let refresh_diagnostics = params
        .capabilities
        .workspace
        .as_ref()
        .and_then(|workspace| workspace.diagnostic.as_ref())
        .and_then(|diagnostic| diagnostic.refresh_support)
        .unwrap_or(false);
    let mut workspace = WorkspaceState::default();
    let mut roots = params
        .workspace_folders
        .unwrap_or_default()
        .into_iter()
        .map(|folder| folder.uri)
        .collect::<Vec<_>>();
    #[allow(deprecated)]
    if roots.is_empty() {
        if let Some(root_uri) = params.root_uri {
            roots.push(root_uri);
        } else if let Some(root_path) = params.root_path {
            match path_to_uri(Path::new(&root_path)) {
                Ok(uri) => roots.push(uri),
                Err(error) => log_file(&error),
            }
        }
    }
    for root in roots {
        for error in workspace.add_root(root) {
            log_file(&error);
        }
    }
    let mut lsp_context = LspContext {
        workspace,
        register_file_watcher,
        refresh_diagnostics,
        next_request_id: 0,
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
                            Err(e) => {
                                log_file(&format!("Request failed: {e}"));
                                connection
                                    .sender
                                    .send(error(&e, req_id))
                                    .or(Err("Failed to send"))
                            }
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
                    notification::Initialized::METHOD => {
                        let _: lsp_types::InitializedParams =
                            not.extract(Initialized::METHOD).unwrap();
                        if lsp_context.register_file_watcher {
                            let registration = RegistrationParams {
                                registrations: vec![Registration {
                                    id: "ebnfer-watch-ebnf".to_string(),
                                    method: DidChangeWatchedFiles::METHOD.to_string(),
                                    register_options: Some(
                                        serde_json::to_value(
                                            lsp_types::DidChangeWatchedFilesRegistrationOptions {
                                                watchers: vec![FileSystemWatcher {
                                                    glob_pattern: GlobPattern::String(
                                                        "**/*.ebnf".to_string(),
                                                    ),
                                                    kind: None,
                                                }],
                                            },
                                        )
                                        .expect("Failed to serialize file watcher"),
                                    ),
                                }],
                            };
                            let request = ServerRequest {
                                id: RequestId::from("ebnfer-watch-ebnf".to_string()),
                                method: RegisterCapability::METHOD.to_string(),
                                params: serde_json::to_value(registration)
                                    .expect("Failed to serialize registration"),
                            };
                            connection
                                .sender
                                .send(Message::Request(request))
                                .or(Err("Failed to register file watcher"))?;
                        }
                    }
                    notification::DidOpenTextDocument::METHOD => {
                        let params: DidOpenTextDocumentParams =
                            not.extract(DidOpenTextDocument::METHOD).unwrap();
                        log_file(&format!("{params:?}"));
                        lsp_context
                            .workspace
                            .open_document(params.text_document.uri, params.text_document.text);
                        request_diagnostic_refresh(&mut lsp_context, &connection)?;
                    }
                    notification::DidChangeTextDocument::METHOD => {
                        let params: lsp_types::DidChangeTextDocumentParams = not
                            .extract(lsp_types::notification::DidChangeTextDocument::METHOD)
                            .unwrap();
                        log_file(&format!("{params:?}"));
                        if let Some(first_change) = params.content_changes.into_iter().next() {
                            lsp_context
                                .workspace
                                .change_document(params.text_document.uri, first_change.text);
                            request_diagnostic_refresh(&mut lsp_context, &connection)?;
                        } else {
                            log_file("Ignored didChange notification without content changes");
                        }
                    }
                    notification::DidCloseTextDocument::METHOD => {
                        let params: lsp_types::DidCloseTextDocumentParams =
                            not.extract(DidCloseTextDocument::METHOD).unwrap();
                        for error in lsp_context
                            .workspace
                            .close_document(&params.text_document.uri)
                        {
                            log_file(&error);
                        }
                        request_diagnostic_refresh(&mut lsp_context, &connection)?;
                    }
                    notification::DidChangeWatchedFiles::METHOD => {
                        let params: lsp_types::DidChangeWatchedFilesParams =
                            not.extract(DidChangeWatchedFiles::METHOD).unwrap();
                        for change in params.changes {
                            for error in lsp_context
                                .workspace
                                .update_watched_file(change.uri, change.typ)
                            {
                                log_file(&error);
                            }
                        }
                        request_diagnostic_refresh(&mut lsp_context, &connection)?;
                    }
                    notification::DidChangeWorkspaceFolders::METHOD => {
                        let params: lsp_types::DidChangeWorkspaceFoldersParams =
                            not.extract(DidChangeWorkspaceFolders::METHOD).unwrap();
                        for folder in params.event.removed {
                            lsp_context.workspace.remove_root(&folder.uri);
                        }
                        for folder in params.event.added {
                            for error in lsp_context.workspace.add_root(folder.uri) {
                                log_file(&error);
                            }
                        }
                        request_diagnostic_refresh(&mut lsp_context, &connection)?;
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

fn request_diagnostic_refresh(
    lsp_context: &mut LspContext,
    connection: &Connection,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    if !lsp_context.refresh_diagnostics {
        return Ok(());
    }
    let id = format!("ebnfer-diagnostics-{}", lsp_context.next_request_id);
    lsp_context.next_request_id += 1;
    connection.sender.send(Message::Request(ServerRequest {
        id: RequestId::from(id),
        method: WorkspaceDiagnosticRefresh::METHOD.to_string(),
        params: serde_json::to_value(()).expect("Failed to serialize diagnostic refresh"),
    }))?;
    Ok(())
}

fn response<T: serde::Serialize>(id: RequestId, result: T) -> Result<Message, String> {
    let result = serde_json::to_value(result).map_err(|error| error.to_string())?;
    Ok(Message::Response(Response {
        id,
        result: Some(result),
        error: None,
    }))
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
    let uri = params.text_document_position_params.text_document.uri;
    let loc = crate::lsp::Location::from(params.text_document_position_params.position);
    let Some(symbol) = lsp_context.workspace.symbol(&uri, &loc) else {
        return response(id, Option::<lsp_types::GotoDefinitionResponse>::None);
    };
    let def_length = symbol.len();
    let definitions = lsp_context.workspace.definitions(&uri, symbol);
    let resp = if definitions.is_empty() {
        None
    } else {
        Some(lsp_types::GotoDefinitionResponse::Array(
            definitions
                .into_iter()
                .map(|definition| {
                    lsp_types::Location::from_with_uri_length(
                        definition.location,
                        definition.uri,
                        def_length,
                    )
                })
                .collect(),
        ))
    };
    response(id, resp)
}

fn symbols(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::DocumentSymbolParams) =
        extract_req(msg, DocumentSymbolRequest::METHOD);
    let uri = params.text_document.uri;
    let ctx = lsp_context
        .workspace
        .document(&uri)
        .ok_or(format!("{} not found", uri.as_str()))?;
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
                        line: x.1.line as u32,
                        character: x.1.col as u32 + x.0.len() as u32,
                    },
                },
                children: Some(
                    ctx.alternative_definitions(&x.1)
                        .into_iter()
                        .flat_map(|y| {
                            let alternative_hovers = ctx.hover_alternatives(&y);
                            let doc_symbols: Vec<DocumentSymbol> = alternative_hovers
                                .into_iter()
                                .map(|_| {
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
                                                character: y.col as u32 + x.0.len() as u32,
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
        #[allow(clippy::mutable_key_type)]
        let mut changes: HashMap<Uri, Vec<lsp_types::TextEdit>> = HashMap::new();
        let p = Position {
            line: loc.line as u32,
            character: loc.col as u32,
        };
        let range = Range { start: p, end: p };
        changes.insert(
            params.text_document.uri.clone(),
            vec![lsp_types::TextEdit {
                range,
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
    let uri = params.text_document.uri;
    let Some(tokens) = lsp_context.workspace.root_rule(&uri) else {
        return response(id, Option::<SemanticTokensResult>::None);
    };
    let symbol = lsp_context
        .workspace
        .symbol(&uri, &tokens.location)
        .ok_or("No root rule symbol found")?;
    let result = SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
        result_id: None,
        data: vec![SemanticToken {
            delta_line: tokens.location.line as u32,
            delta_start: tokens.location.col as u32,
            length: symbol.len() as u32,
            token_type: 0,
            token_modifiers_bitset: 0,
        }],
    });
    response(id, Some(result))
}

fn rename_prepare(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, lsp_types::TextDocumentPositionParams) =
        extract_req(msg, PrepareRenameRequest::METHOD);
    let uri = params.text_document.uri;
    let loc = crate::lsp::Location::from(params.position);
    let result = match lsp_context.workspace.symbol(&uri, &loc) {
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
    let uri = params.text_document_position.text_document.uri;
    let loc = crate::lsp::Location::from(params.text_document_position.position);
    let symbol = lsp_context
        .workspace
        .symbol(&uri, &loc)
        .ok_or("No symbol found")?;
    let symbol_len = symbol.len();
    let all_locs = lsp_context
        .workspace
        .definitions(&uri, symbol)
        .into_iter()
        .chain(lsp_context.workspace.references(&uri, symbol));
    let new_name = params.new_name;
    #[allow(clippy::mutable_key_type)]
    let mut edits: HashMap<Uri, Vec<lsp_types::TextEdit>> = HashMap::new();
    for document_location in all_locs {
        let location = document_location.location;
        edits
            .entry(document_location.uri)
            .or_default()
            .push(lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: location.line as u32,
                        character: location.col as u32,
                    },
                    end: lsp_types::Position {
                        line: location.line as u32,
                        character: (location.col + symbol_len) as u32,
                    },
                },
                new_text: new_name.clone(),
            });
    }
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
    let uri = params.text_document.uri;
    let ctx = lsp_context
        .workspace
        .document(&uri)
        .ok_or(format!("{} not found", uri.as_str()))?;
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
    let uri = params.text_document_position.text_document.uri;
    let symbols = lsp_context
        .workspace
        .completions(&uri)
        .into_iter()
        .map(|(name, hover)| {
            let mut item = CompletionItem::new_simple(name, "EBNF rule".to_string());
            item.documentation = Some(Documentation::String(hover.to_string()));
            let description = hover
                .split('=')
                .nth(1)
                .unwrap_or_default()
                .trim()
                .to_string();
            item.label_details = Some(lsp_types::CompletionItemLabelDetails {
                detail: None,
                description: Some(description),
            });
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
    let uri = params.text_document_position.text_document.uri;
    let loc = crate::lsp::Location::from(params.text_document_position.position);
    let symbol = lsp_context
        .workspace
        .symbol(&uri, &loc)
        .ok_or("No symbol found")?;
    let defs_len = symbol.len();
    let mut locations = lsp_context.workspace.references(&uri, symbol);
    if params.context.include_declaration {
        locations.extend(lsp_context.workspace.definitions(&uri, symbol));
        locations.sort_by(|a, b| {
            a.uri
                .cmp(&b.uri)
                .then(a.location.line.cmp(&b.location.line))
                .then(a.location.col.cmp(&b.location.col))
        });
    }
    let ref_response: Vec<lsp_types::Location> = locations
        .into_iter()
        .map(|document_location| {
            lsp_types::Location::from_with_uri_length(
                document_location.location,
                document_location.uri,
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

fn diagnostics(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, DocumentDiagnosticParams) =
        extract_req(msg, DocumentDiagnosticRequest::METHOD);
    let items = lsp_context
        .workspace
        .diagnostics(&params.text_document.uri)
        .into_iter()
        .map(Diagnostic::from)
        .collect();
    let report = DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
        RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                items,
                result_id: None,
            },
        },
    ));
    let json_result = serde_json::to_value(report).expect("Failed to serialize");
    Ok(Message::Response(Response {
        id,
        result: Some(json_result),
        error: None,
    }))
}

fn hover(lsp_context: &LspContext, msg: Message) -> Result<Message, String> {
    let (id, params): (RequestId, HoverParams) = extract_req(msg, HoverRequest::METHOD);
    let uri = params.text_document_position_params.text_document.uri;
    let location = Location::from(params.text_document_position_params.position);
    let symbol = match lsp_context.workspace.symbol(&uri, &location) {
        Some(symbol) => symbol,
        None => {
            return Ok(Message::Response(Response {
                id,
                result: None,
                error: None,
            }))
        }
    };
    let mut hovers = lsp_context
        .workspace
        .hovers(&uri, symbol)
        .into_iter()
        .map(|definition| {
            MarkedString::LanguageString(LanguageString {
                language: "ebnf".to_string(),
                value: definition.text.trim().to_string(),
            })
        })
        .collect::<Vec<MarkedString>>();

    let resp = match hovers.len() {
        0 => {
            return response(id, Option::<Hover>::None);
        }
        1 => Hover {
            range: None,
            contents: lsp_types::HoverContents::Scalar(hovers.remove(0)),
        },
        _ => Hover {
            range: None,
            contents: lsp_types::HoverContents::Array(hovers),
        },
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
