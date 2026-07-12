import { execFileSync } from "node:child_process";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const [vscodeTarget, executablePath, requestedOutput] = process.argv.slice(2);
if (!vscodeTarget || !executablePath) {
  throw new Error(
    "Usage: node scripts/package-target.mjs <vscode-target> <executable-path> [output.vsix]",
  );
}

const extensionRoot = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
const output = requestedOutput ?? `ebnfer-${vscodeTarget}.vsix`;
const executableName = process.platform === "win32" ? "ebnfer.exe" : "ebnfer";
const npx = process.platform === "win32" ? "npx.cmd" : "npx";

execFileSync(
  process.execPath,
  [path.join(extensionRoot, "scripts", "stage-server.mjs"), executablePath],
  { cwd: extensionRoot, stdio: "inherit" },
);
execFileSync(
  npx,
  ["vsce", "package", "--target", vscodeTarget, "--out", output],
  { cwd: extensionRoot, stdio: "inherit" },
);
execFileSync(
  process.execPath,
  [
    path.join(extensionRoot, "scripts", "verify-vsix.mjs"),
    output,
    executableName,
  ],
  { cwd: extensionRoot, stdio: "inherit" },
);
