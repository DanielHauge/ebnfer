pub mod ipc {

    #![allow(clippy::print_stderr)]

    use std::error::Error;

    use lsp_types::{
        request::GotoDefinition, DiagnosticOptions, DiagnosticServerCapabilities,
        GotoDefinitionResponse, InitializeParams, ServerCapabilities,
    };
    use lsp_types::{HoverProviderCapability, OneOf};

    use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
    use serde_json::Value;
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
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
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
        log_file(&format!("{_params:?}"));
        for msg in &connection.receiver {
            eprintln!("got msg: {msg:?}");
            log_file(&format!("{msg:?}"));
            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    log_file(&format!("{req:?}"));

                    eprintln!("got request: {req:?}");
                    // match cast::<GotoDefinition>(req) {
                    //     Ok((id, params)) => {
                    //         eprintln!("got gotoDefinition request #{id}: {params:?}");
                    //
                    //         let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                    //         let result = serde_json::to_value(&result).unwrap();
                    //         let resp = Response {
                    //             id,
                    //             result: Some(result),
                    //             error: None,
                    //         };
                    //         connection.sender.send(Message::Response(resp))?;
                    //         continue;
                    //     }
                    //     Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    //     Err(ExtractError::MethodMismatch(req)) => req,
                    // };
                    // ...
                }
                Message::Response(resp) => {
                    log_file(&format!("{resp:?}"));

                    eprintln!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    log_file(&format!("{not:?}"));

                    eprintln!("got notification: {not:?}");
                }
            }
        }
        Ok(())
    }

    fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        req.extract(R::METHOD)
    }
}
