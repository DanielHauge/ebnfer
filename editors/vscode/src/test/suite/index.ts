import * as assert from "node:assert/strict";
import * as path from "node:path";

import * as vscode from "vscode";

async function waitFor<T>(
  read: () => T | undefined | Promise<T | undefined>,
  timeoutMilliseconds = 10_000,
): Promise<T> {
  const deadline = Date.now() + timeoutMilliseconds;
  while (Date.now() < deadline) {
    const value = await read();
    if (value !== undefined) {
      return value;
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error("Timed out waiting for VS Code language-server result");
}

export async function run(): Promise<void> {
  const extension = vscode.extensions.getExtension("DanielHauge.ebnfer");
  assert.ok(extension, "Extension was not discovered");
  await extension.activate();

  const workspace = vscode.workspace.workspaceFolders?.[0];
  assert.ok(workspace, "Fixture workspace was not opened");
  const startUri = vscode.Uri.file(
    path.join(workspace.uri.fsPath, "start.ebnf"),
  );
  const itemUri = vscode.Uri.file(path.join(workspace.uri.fsPath, "item.ebnf"));
  const startDocument = await vscode.workspace.openTextDocument(startUri);
  await vscode.window.showTextDocument(startDocument);
  assert.equal(startDocument.languageId, "ebnf");

  const definitions = await waitFor(async () => {
    const result = await vscode.commands.executeCommand<
      Array<vscode.Location | vscode.LocationLink>
    >(
      "vscode.executeDefinitionProvider",
      startUri,
      new vscode.Position(0, 9),
    );
    return result && result.length > 0 ? result : undefined;
  });
  const definition = definitions[0];
  assert.ok(definition);
  const definitionUri =
    "uri" in definition ? definition.uri : definition.targetUri;
  assert.equal(definitionUri.toString(), itemUri.toString());

  const diagnostics = await waitFor(() => {
    const current = vscode.languages.getDiagnostics(startUri);
    return current.some((diagnostic) =>
      diagnostic.message.includes("Undefined reference: Missing"),
    )
      ? current
      : undefined;
  });
  assert.ok(diagnostics.length > 0);
}
