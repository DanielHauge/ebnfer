import { constants } from "node:fs";
import { access } from "node:fs/promises";
import * as path from "node:path";

export interface RuntimePlatform {
  platform: NodeJS.Platform;
  arch: string;
}

export function bundledExecutableName(platform: NodeJS.Platform): string {
  return platform === "win32" ? "ebnfer.exe" : "ebnfer";
}

export function resolveServerPath(
  extensionPath: string,
  configuredPath: string | undefined,
  workspacePath: string | undefined,
  runtime: RuntimePlatform = {
    platform: process.platform,
    arch: process.arch,
  },
): string {
  const override = configuredPath?.trim();
  if (override) {
    return path.isAbsolute(override)
      ? override
      : path.resolve(workspacePath ?? extensionPath, override);
  }

  if (!["x64", "arm64"].includes(runtime.arch)) {
    throw new Error(`Unsupported architecture: ${runtime.arch}`);
  }
  if (!["linux", "darwin", "win32"].includes(runtime.platform)) {
    throw new Error(`Unsupported platform: ${runtime.platform}`);
  }

  return path.join(extensionPath, "server", bundledExecutableName(runtime.platform));
}

export async function validateServerPath(
  serverPath: string,
  platform: NodeJS.Platform = process.platform,
): Promise<void> {
  try {
    await access(
      serverPath,
      platform === "win32" ? constants.F_OK : constants.X_OK,
    );
  } catch (error) {
    throw new Error(
      `Server executable is unavailable at ${serverPath}: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
}
