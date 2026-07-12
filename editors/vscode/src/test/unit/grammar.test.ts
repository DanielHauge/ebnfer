import * as assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import * as path from "node:path";
import test from "node:test";

import { loadWASM, OnigScanner, OnigString } from "vscode-oniguruma";
import { parseRawGrammar, Registry } from "vscode-textmate";

const extensionRoot = path.resolve(__dirname, "../../..");

async function createRegistry(): Promise<Registry> {
  const wasm = await readFile(
    require.resolve("vscode-oniguruma/release/onig.wasm"),
  );
  await loadWASM(wasm.buffer);
  const grammarPath = path.join(
    extensionRoot,
    "syntaxes",
    "ebnf.tmLanguage.json",
  );
  const grammarSource = await readFile(grammarPath, "utf8");
  const grammar = parseRawGrammar(grammarSource, grammarPath);

  return new Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (sources) => new OnigScanner(sources),
      createOnigString: (source) => new OnigString(source),
    }),
    loadGrammar: async (scopeName) =>
      scopeName === "source.ebnf" ? grammar : null,
  });
}

test("tokenizes the supported EBNF dialect", async () => {
  const registry = await createRegistry();
  const grammar = await registry.loadGrammar("source.ebnf");
  assert.ok(grammar);

  const source = await readFile(
    path.join(extensionRoot, "test-fixtures", "grammar.ebnf"),
    "utf8",
  );
  const scopedTokens: Array<{ text: string; scopes: string[] }> = [];
  let ruleStack = null;
  for (const line of source.split(/\r?\n/)) {
    const result = grammar.tokenizeLine(line, ruleStack);
    ruleStack = result.ruleStack;
    for (const token of result.tokens) {
      scopedTokens.push({
        text: line.slice(token.startIndex, token.endIndex),
        scopes: token.scopes,
      });
    }
  }

  const hasScope = (text: string, scope: string): boolean =>
    scopedTokens.some(
      (token) => token.text.includes(text) && token.scopes.includes(scope),
    );

  assert.ok(hasScope("grammar comment", "comment.block.ebnf"));
  assert.ok(hasScope("Grammar", "entity.name.function.rule.ebnf"));
  assert.ok(hasScope("Rule", "variable.other.rule-reference.ebnf"));
  assert.ok(hasScope("2", "constant.numeric.integer.ebnf"));
  assert.ok(hasScope("double terminal", "string.quoted.double.ebnf"));
  assert.ok(hasScope("single terminal", "string.quoted.single.ebnf"));
  assert.ok(
    hasScope("special sequence", "string.other.special-sequence.ebnf"),
  );
  assert.ok(hasScope("=", "keyword.operator.ebnf"));
});
