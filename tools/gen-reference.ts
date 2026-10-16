// Development only. Renders every test/cases/*.json with WaveDrom itself and
// writes the result to test/reference/<name>.svg, the oracle the ERT suite
// compares against.  Usage: bun tools/gen-reference.ts

import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const root = join(import.meta.dir, "node_modules", "wavedrom");
const wd = require(root);

const skinDir = join(root, "skins");
const allSkins: Record<string, any> = {};
for (const f of readdirSync(skinDir).filter((f) => f.endsWith(".js")).sort()) {
  Object.assign(allSkins, require(join(skinDir, f)));
}

const cases = join(import.meta.dir, "..", "test", "cases");
const refs = join(import.meta.dir, "..", "test", "reference");

for (const f of readdirSync(cases).filter((f) => f.endsWith(".json")).sort()) {
  const text = readFileSync(join(cases, f), "utf8");
  // WaveDrom renders mutate both the source and the skin, so each case
  // starts from fresh copies.
  const source = (0, eval)("(" + text + ")");
  const skins = structuredClone(allSkins);
  const svg = wd.onml.stringify(wd.renderAny(0, source, skins));
  writeFileSync(join(refs, f.replace(/\.json$/, ".svg")), svg);
  console.log(`${f} -> ${svg.length} bytes`);
}
