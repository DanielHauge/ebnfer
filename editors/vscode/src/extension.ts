import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import { resolveServerPath, validateServerPath } from "./serverPath";

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const outputChannel = vscode.window.createOutputChannel("EBNFER", {
    log: true,
  });
  context.subscriptions.push(outputChannel);

  const configuredPath = vscode.workspace
    .getConfiguration("ebnfer")
    .get<string>("server.path");
  const workspacePath = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

  let command: string;
  try {
    command = resolveServerPath(context.extensionPath, configuredPath, workspacePath);
    await validateServerPath(command);
  } catch (error) {
    const message = `Unable to start EBNFER: ${error instanceof Error ? error.message : String(error)}`;
    outputChannel.appendLine(message);
    await vscode.window.showErrorMessage(message);
    throw error;
  }

  const executable: Executable = {
    command,
    transport: TransportKind.stdio,
  };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { language: "ebnf", scheme: "file" },
      { language: "ebnf", scheme: "untitled" },
    ],
    outputChannel,
  };

  client = new LanguageClient("ebnfer", "EBNFER", serverOptions, clientOptions);
  await client.start();
}

export async function deactivate(): Promise<void> {
  await client?.dispose();
  client = undefined;
}
