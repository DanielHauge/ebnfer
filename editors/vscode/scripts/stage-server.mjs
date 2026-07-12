import { chmod, copyFile, mkdir, rm } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const source = process.argv[2];
if (!source) {
  throw new Error("Usage: node scripts/stage-server.mjs <executable>");
}

const extensionRoot = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
const serverDirectory = path.join(extensionRoot, "server");
const executableName = process.platform === "win32" ? "ebnfer.exe" : "ebnfer";
const destination = path.join(serverDirectory, executableName);

await mkdir(serverDirectory, { recursive: true });
await Promise.all(
  ["ebnfer", "ebnfer.exe"].map((name) =>
    rm(path.join(serverDirectory, name), { force: true }),
  ),
);
await copyFile(path.resolve(source), destination);
if (process.platform !== "win32") {
  await chmod(destination, 0o755);
}

console.log(destination);
