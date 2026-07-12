import { execFileSync } from "node:child_process";
import { access, readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

import AdmZip from "adm-zip";

const targets = [
  "linux-x64",
  "linux-arm64",
  "darwin-x64",
  "darwin-arm64",
  "win32-x64",
  "win32-arm64",
];

const extensionRoot = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
const arguments_ = process.argv.slice(2);
const dryRunIndex = arguments_.indexOf("--dry-run");
const dryRun = dryRunIndex !== -1;
if (dryRun) {
  arguments_.splice(dryRunIndex, 1);
}
const skipDuplicateIndex = arguments_.indexOf("--skip-duplicate");
const skipDuplicate = skipDuplicateIndex !== -1;
if (skipDuplicate) {
  arguments_.splice(skipDuplicateIndex, 1);
}

const artifactsDirectory = path.resolve(arguments_[0] ?? ".");
const packageJson = JSON.parse(
  await readFile(path.join(extensionRoot, "package.json"), "utf8"),
);
const packages = targets.map((target) =>
  path.join(artifactsDirectory, `ebnfer-${target}.vsix`),
);

for (const [index, packagePath] of packages.entries()) {
  await access(packagePath);
  const archive = new AdmZip(packagePath);
  const manifest = archive.getEntry("extension/package.json");
  if (!manifest) {
    throw new Error(`${packagePath} does not contain extension/package.json`);
  }
  const packagedMetadata = JSON.parse(manifest.getData().toString("utf8"));
  if (
    packagedMetadata.name !== packageJson.name ||
    packagedMetadata.publisher !== packageJson.publisher ||
    packagedMetadata.version !== packageJson.version
  ) {
    throw new Error(
      `${packagePath} metadata does not match ${packageJson.publisher}.${packageJson.name}@${packageJson.version}`,
    );
  }

  const expectedExecutable = targets[index]?.startsWith("win32-")
    ? "extension/server/ebnfer.exe"
    : "extension/server/ebnfer";
  const executables = archive
    .getEntries()
    .filter((entry) => entry.entryName.startsWith("extension/server/ebnfer"));
  if (
    executables.length !== 1 ||
    executables[0]?.entryName !== expectedExecutable
  ) {
    throw new Error(
      `${packagePath} must contain exactly ${expectedExecutable}`,
    );
  }
}

console.log(
  `Validated ${packages.length} packages for ${packageJson.publisher}.${packageJson.name}@${packageJson.version}`,
);

const commandArguments = [
  "vsce",
  "publish",
  "--packagePath",
  ...packages,
];
if (skipDuplicate) {
  commandArguments.push("--skip-duplicate");
}

if (dryRun) {
  console.log(
    `${process.platform === "win32" ? "npx.cmd" : "npx"} ${commandArguments.join(" ")}`,
  );
  process.exit(0);
}

if (!process.env.VSCE_PAT) {
  throw new Error(
    "VSCE_PAT is not set. Create a Marketplace token with Marketplace > Manage permission.",
  );
}

execFileSync(process.platform === "win32" ? "npx.cmd" : "npx", commandArguments, {
  cwd: extensionRoot,
  env: process.env,
  stdio: "inherit",
});
