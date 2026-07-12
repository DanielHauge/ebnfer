import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { fileURLToPath } from "node:url";

const extensionRoot = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
const repositoryRoot = path.resolve(extensionRoot, "../..");

const packageJson = JSON.parse(
  await readFile(path.join(extensionRoot, "package.json"), "utf8"),
);
const cargoToml = await readFile(path.join(repositoryRoot, "Cargo.toml"), "utf8");
const cargoVersion = cargoToml.match(
  /^\s*version\s*=\s*"([^"]+)"\s*$/m,
)?.[1];

if (!cargoVersion) {
  throw new Error("Unable to read the package version from Cargo.toml");
}
if (cargoVersion !== packageJson.version) {
  throw new Error(
    `Version mismatch: Cargo.toml=${cargoVersion}, package.json=${packageJson.version}`,
  );
}

const tag = process.env.GITHUB_REF?.startsWith("refs/tags/")
  ? process.env.GITHUB_REF_NAME
  : process.argv[2];
if (tag) {
  const tagVersion = tag.replace(/^v/, "");
  if (tagVersion !== cargoVersion) {
    throw new Error(
      `Version mismatch: release tag=${tagVersion}, package version=${cargoVersion}`,
    );
  }
}
