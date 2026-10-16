---
phase: complete
progress: 14/14
task: "Render WaveDrom WaveJSON timing diagrams inside Emacs with no third-party runtime"
slug: ewavedrom
iteration: 1
started: 2026-10-16T00:00:00Z
updated: 2026-10-16T00:00:00Z
principal_stated_goal: "i like the emacs list only idea. having python (with no external dependencies other the python pkgs) would be a preferred fallback solution for me. Lets start with your recommendation and you can include adges and arrows on v1."
principal_stated_goal_source: prompt
principal_stated_goal_signal: 1
principal_stated_goal_locked: 2026-10-16T00:00:00Z
---

# ewavedrom -- WaveDrom timing diagrams in pure Emacs Lisp

## Problem

VS Code renders WaveDrom `.json` timing diagrams through a plug-in; Emacs has only
`wavedrom-mode`, which shells out to `wavedrom-cli` and therefore needs Node.js.
The work environment forbids most third-party runtimes, so Node is not an option.

## Vision

Open a WaveJSON file at work, press one key, and the diagram appears next to it,
looking exactly as WaveDrom draws it, with nothing installed but Emacs. The same
source block in an Org file renders inline and exports as an SVG file.

## Out of Scope

`reg` (bitfield) and `assign` (logic circuit) diagrams; the Python fallback (named
by the author as the preferred plan B, not built in v1); PNG/PDF export; editing aids
beyond font-lock inherited from `js-mode`.

## Principles

The renderer is a faithful port of WaveDrom 3.7.0 (MIT), so WaveDrom itself is the
test oracle: the same WaveJSON must give the same SVG bytes. Node and bun appear
only in `tools/`, which regenerates vendored data and reference SVGs at home; the
runtime never touches them.

## Constraints

- Runtime needs Emacs only, built with SVG (librsvg) support for display.
- No `call-process`, `make-process`, network, or external program at runtime.
- Target Emacs 27.1 or later (no 28+ only functions in runtime code).
- RTL rule does not apply; documentation in Org (new project rule).

## Goal

A pure Emacs Lisp package that reads WaveJSON as WaveDrom accepts it, renders it to
the same SVG WaveDrom 3.7.0 produces, including edges and arrows, and shows it in
Emacs from a WaveJSON buffer and from Org source blocks.

## Claims

- [x] ISC-1: The reader accepts WaveDrom's JavaScript-literal input: unquoted keys,
      single and double quoted strings with escapes, trailing commas, `//` and
      `/* */` comments, signed and fractional numbers, `true`/`false`/`null`. -- `ERT read-js-literal, read-escapes-and-unicode`
- [x] ISC-2: Malformed input raises a `user-error` naming the line and column,
      never a Lisp backtrace. -- `ERT read-error-names-position, read-error-unterminated`
- [x] ISC-3: For every case in `test/cases/`, the SVG string equals WaveDrom
      3.7.0's `onml.stringify(renderAny(0, src, skins))` byte for byte. -- `ERT golden-* (24 cases); planted ref mismatch caught`
- [x] ISC-4: The corpus covers every wave character (`p n P N h l H L 0 1 x z = 2-9 d u . |`),
      `data` as array and string, `period`, `phase`, `hscale`, sub-cycles `<>`,
      named and nested groups, empty spacer lanes, and `head`/`foot` text, tick, tock
      and `every`. -- `ERT corpus-covers-features`
- [x] ISC-5: Edges and arrows: the corpus exercises every arc shape in WaveDrom's
      `arc-shape.js` (all 20 cases plus an unknown shape), labelled and unlabelled,
      and node labels. -- `ERT golden-edges-*, corpus-covers-every-arc-shape`
- [x] ISC-6: All six WaveDrom skins are available through `config.skin` and match
      the reference for each. -- `ERT golden-skin-* (6), corpus-covers-every-skin`
- [x] ISC-7: `M-x ewavedrom-preview` on a WaveJSON buffer displays the diagram in
      a side window as non-degenerate pixels in a GUI Emacs. -- `GUI Emacs 31.1 frame export, preview fitted 660->436 px`
- [x] ISC-8: `ewavedrom-preview-mode` re-renders on save; an error keeps the last
      good image and reports the error in the echo area. -- `ERT preview-keeps-last-good-image, preview-mode-renders-on-save`
- [x] ISC-9: An Org `#+begin_src wavedrom :file out.svg` block writes the SVG file,
      and a block without `:file` returns the SVG markup (for `:results html` export). -- `ERT babel-file-and-inline`
- [x] ISC-10: `ewavedrom-render-file` writes an SVG that `rsvg-convert` (the same
      librsvg Emacs uses) rasterises to non-degenerate pixels. -- `make png: 24 PNGs, SVGs cmp-identical to references`
- [x] ISC-11: Byte-compilation of every runtime file emits zero warnings. -- `make compile, byte-compile-error-on-warn, Emacs 31.1 and 27.1`
- [x] ISC-12: Anti: no runtime file calls a process, the network, or loads a
      non-built-in library; checked by grep with a planted positive. -- `make audit, planted call-process caught`
- [x] ISC-13: Anti: no runtime file uses a function introduced after Emacs 27.1,
      checked against a planted positive. -- `silex/emacs:27.1 container: compile clean, 38/38`
- [x] ISC-14: README.org documents install, usage, the dependency list (Emacs
      version, librsvg) and the WaveDrom MIT attribution; LICENSE carries both
      copyright lines. -- `README.org, LICENSE`

## Test Strategy

| isc | type | check | threshold | tool | anchors_to |
|-----|------|-------|-----------|------|------------|
| ISC-1 | unit | ERT feeds each syntax form to the reader | all parse to expected values | `make test` | derived: WaveDrom evals JS |
| ISC-2 | unit | ERT asserts the error message carries line:col | match | `make test` | derived: usable at work |
| ISC-3 | golden | ERT compares render output with `test/reference/*.svg` | identical strings | `make test` | literal: "render waveforms" |
| ISC-4 | coverage | ERT asserts the corpus contains each listed feature | all present | `make test` | literal |
| ISC-5 | golden | corpus cases `edges-*` against reference | identical | `make test` | literal: "edges and arrows on v1" |
| ISC-6 | golden | one case per skin | identical | `make test` | derived |
| ISC-7 | visual | GUI Emacs under Wayland, screenshot with `grim` | diagram visible | `grim`, Read | literal |
| ISC-8 | unit | ERT drives save hook with good then bad input | last image kept, message shown | `make test` | derived |
| ISC-9 | unit | ERT runs `org-babel-execute-src-block` | file written / image returned | `make test` | literal: Emacs workflow |
| ISC-10 | visual | `make png` then view PNG | non-degenerate | `rsvg-convert`, Read | derived |
| ISC-11 | build | `make compile` with `byte-compile-error-on-warn` | 0 warnings | `make compile` | derived |
| ISC-12 | grep | `make audit` pattern scan + planted file | 0 real, 1 planted hit | `make audit` | literal: minimal deps |
| ISC-13 | grep | `make audit` scans for 28+ functions + planted file | 0 real, 1 planted hit | `make audit` | literal: restricted work env |
| ISC-14 | read | read README.org and LICENSE | sections present | Read | derived |

## Decisions

- Port, not reimplementation: WaveDrom's renderer is ~1,500 lines of plain JS and
  its skins are data, so a faithful port gets exact output and an oracle for free.
- Skins and the text-width table are vendored as generated Lisp data
  (`ewavedrom-data.el`) from the
  `wavedrom@3.7.0` npm package by `tools/gen-data.ts`.
- JS objects are read into ordered alists so attribute and key order match JS
  insertion order; integer-like keys are ordered first where WaveDrom iterates
  object keys (node labels), matching `Object.keys`.

- The preview fits the window width by default: a 940-px frame clipped the
  tutorial diagram in the first GUI check.
- `.json` files are recognised by content (`magic-mode-alist`), matching the
  VS Code workflow without renaming files.

## Not yet specified

- Windows Emacs at work: the suite has run on Linux only (Emacs 27.1, 31.1).
