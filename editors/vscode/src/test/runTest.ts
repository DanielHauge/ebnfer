import * as path from "node:path";

import { runTests } from "@vscode/test-electron";

async function main(): Promise<void> {
  const extensionDevelopmentPath = path.resolve(__dirname, "../..");
  const extensionTestsPath = path.resolve(__dirname, "suite", "index");
  const fixturePath = path.join(
    extensionDevelopmentPath,
    "test-fixtures",
    "workspace",
  );

  await runTests({
    version: "1.91.0",
    extensionDevelopmentPath,
    extensionTestsPath,
    launchArgs: [fixturePath, "--disable-extensions", "--disable-gpu"],
  });
}

main().catch((error: unknown) => {
  console.error(error);
  process.exitCode = 1;
});
