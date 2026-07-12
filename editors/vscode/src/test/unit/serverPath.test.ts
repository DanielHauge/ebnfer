import * as assert from "node:assert/strict";
import { chmod, mkdtemp, rm, writeFile } from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";

import {
  bundledExecutableName,
  resolveServerPath,
  validateServerPath,
} from "../../serverPath";

test("selects the platform executable name", () => {
  assert.equal(bundledExecutableName("linux"), "ebnfer");
  assert.equal(bundledExecutableName("darwin"), "ebnfer");
  assert.equal(bundledExecutableName("win32"), "ebnfer.exe");
});

test("resolves the bundled executable", () => {
  assert.equal(
    resolveServerPath("/extension", "", undefined, {
      platform: "linux",
      arch: "x64",
    }),
    path.join("/extension", "server", "ebnfer"),
  );
});

test("resolves relative overrides from the workspace", () => {
  assert.equal(
    resolveServerPath("/extension", "./bin/ebnfer", "/workspace", {
      platform: "linux",
      arch: "x64",
    }),
    path.resolve("/workspace", "bin/ebnfer"),
  );
});

test("rejects unsupported bundled platforms", () => {
  assert.throws(
    () =>
      resolveServerPath("/extension", undefined, undefined, {
        platform: "freebsd",
        arch: "x64",
      }),
    /Unsupported platform/,
  );
  assert.throws(
    () =>
      resolveServerPath("/extension", undefined, undefined, {
        platform: "linux",
        arch: "riscv64",
      }),
    /Unsupported architecture/,
  );
});

test("validates executable availability", async () => {
  const directory = await mkdtemp(path.join(os.tmpdir(), "ebnfer-vscode-"));
  const executable = path.join(directory, "ebnfer");
  try {
    await writeFile(executable, "");
    await chmod(executable, 0o755);
    await validateServerPath(executable, "linux");
    await assert.rejects(
      validateServerPath(path.join(directory, "missing"), "linux"),
      /Server executable is unavailable/,
    );
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
