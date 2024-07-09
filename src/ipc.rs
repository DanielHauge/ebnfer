pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::collections::HashMap;
    use std::error::Error;

    use lsp_types::notification::{self, DidOpenTextDocument, Notification};
    use lsp_types::request::HoverRequest;
    use lsp_types::{
        DidOpenTextDocumentParams, Hover, HoverParams, HoverProviderCapability, OneOf,
        TextDocumentSyncCapability, TextDocumentSyncKind,
    };
    use lsp_types::{InitializeParams, ServerCapabilities};

    use lsp_server::{
        Connection, ExtractError, Message, Request, RequestId, Response, ResponseError,
    };
    use serde_json::Value;

    use crate::ebnf::ebnf::parse_ebnf;
    use crate::lsp::lsp::Location;
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
                        Ok((id, hov_req)) => match hover(&docs, id.clone(), hov_req) {
                            Ok(x) => connection.sender.send(x).unwrap(),
                            Err(e) => connection.sender.send(error(&e, id)).unwrap(),
                        },
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
                            docs.insert(
                                params.text_document.uri.to_string(),
                                params.content_changes[0].text.to_string(),
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

    fn hover(
        docs: &HashMap<String, String>,
        id: RequestId,
        params: HoverParams,
    ) -> Result<Message, String> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let doc = docs.get(&uri).ok_or("Document not found")?;
        // Log doc to file
        log_file(&format!("Document: {doc:?}"));
        let lsp_context = match parse_ebnf(doc) {
            Ok(ctx) => ctx.1.lsp_context,
            Err(e) => return Err(format!("Document Error: {:?}", e)),
        };
        let location = Location {
            line: params.text_document_position_params.position.line as usize,
            col: params.text_document_position_params.position.character as usize,
        };
        let hover = lsp_context
            .hover(location)
            .ok_or("No hover information found")?;
        let result = Some(Hover {
            range: None,
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                hover.to_string(),
            )),
        });
        let json_result = serde_json::to_value(result).map_err(|e| e.to_string())?;
        Ok(Message::Response(Response {
            id,
            result: Some(json_result),
            error: None,
        }))
    }

    fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        req.extract(R::METHOD)
    }
}
