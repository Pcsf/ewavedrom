# ewavedrom --- build, test and audit
#
# Runtime needs Emacs only.  The data and refs targets need bun and the
# pinned WaveDrom in tools/, and are run at home, never on the target machine.

EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .
EL     = ewavedrom-data.el ewavedrom.el ob-ewavedrom.el
BUILD  = build

.PHONY: all compile test audit png data refs clean

all: compile test audit

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

test: compile
	$(BATCH) -l test/ewavedrom-test.el -f ert-run-tests-batch-and-exit

# Runtime files must not reach outside Emacs, nor use functions newer than
# Emacs 27.1.  Each scan also runs against a planted file and must hit it,
# so an empty result cannot come from a broken pattern.
FORBIDDEN_CALLS = (call-process|process-file|start-process|make-process|shell-command|url-retrieve|open-network-stream|make-network-process)
EMACS28_FNS     = (string-search|string-replace|length[<>=]|ensure-list|string-pad|string-fill|string-chop-newline|file-name-concat|take |ntake |pos-bol|pos-eol|json-parse|seq-keep|plistp|always )
audit: | $(BUILD)
	@printf '(call-process "x")\n(string-search "a" "b")\n' > $(BUILD)/planted.el
	@test "$$(grep -cE '$(FORBIDDEN_CALLS)' $(BUILD)/planted.el)" = 1 || { echo "audit: process pattern is broken"; exit 1; }
	@test "$$(grep -cE '\($(EMACS28_FNS)' $(BUILD)/planted.el)" = 1 || { echo "audit: version pattern is broken"; exit 1; }
	@if grep -nE '$(FORBIDDEN_CALLS)' $(EL); then echo "audit: runtime reaches outside Emacs"; exit 1; fi
	@if grep -nE '\($(EMACS28_FNS)' $(EL); then echo "audit: runtime needs Emacs 28+"; exit 1; fi
	@if grep -nE "^\(require '" $(EL) | grep -vE "'(cl-lib|subr-x|ewavedrom|ewavedrom-data|org|ob|ob-core|image|svg)\)"; then \
	  echo "audit: unexpected require"; exit 1; fi
	@echo "audit: planted cases caught, runtime clean"

# Rasterises every reference and every ewavedrom render with librsvg, the
# library Emacs itself displays SVG with.
png: compile | $(BUILD)
	@for f in test/cases/*.json; do \
	  n=$$(basename $$f .json); \
	  $(BATCH) -l ewavedrom --eval "(ewavedrom-render-file \"$$f\" \"$(BUILD)/$$n.svg\")" && \
	  rsvg-convert -o $(BUILD)/$$n.png $(BUILD)/$$n.svg || exit 1; \
	done
	@echo "PNGs in $(BUILD)/"

data:
	cd tools && bun install --frozen-lockfile
	bun tools/gen-data.ts > ewavedrom-data.el

refs:
	cd tools && bun install --frozen-lockfile
	bun tools/gen-reference.ts

$(BUILD):
	mkdir -p $(BUILD)

clean:
	rm -rf $(BUILD) *.elc
