import process from "node:process";

import AdmZip from "adm-zip";

const vsix = process.argv[2];
const executable = process.argv[3];
if (!vsix || !executable) {
  throw new Error(
    "Usage: node scripts/verify-vsix.mjs <package.vsix> <ebnfer|ebnfer.exe>",
  );
}

const archive = new AdmZip(vsix);
const serverEntries = archive
  .getEntries()
  .filter((entry) => entry.entryName.startsWith("extension/server/ebnfer"));

if (
  serverEntries.length !== 1 ||
  serverEntries[0]?.entryName !== `extension/server/${executable}`
) {
  throw new Error(
    `Expected exactly server/${executable} in ${vsix}, found:\n${serverEntries.map((entry) => entry.entryName).join("\n")}`,
  );
}

if (executable === "ebnfer") {
  const mode = (serverEntries[0].header.attr >>> 16) & 0o777;
  if ((mode & 0o111) === 0) {
    throw new Error(`Bundled server is not executable (mode ${mode.toString(8)})`);
  }
}
