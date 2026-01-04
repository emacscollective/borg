-include ../config.mk
include ../default.mk

.PHONY: stats

docs: texi info html html-dir pdf

texi:     $(PKG).texi
info:     $(PKG).info dir
html:     $(PKG).html
html-dir: $(PKG)/index.html
pdf:      $(PKG).pdf

ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "\
(defun org-texinfo--sanitize-content (text)\
  (replace-regexp-in-string \"[@@{}]\" \"@@\\&\" text))"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

redo-docs:
	@touch $(PKG).org
	@make docs

.revdesc: ;
_    := $(shell test "$(REVDESC)" = "$$(cat .revdesc 2> /dev/null)" ||\
        echo "$(REVDESC)" > .revdesc)

%.texi: %.org .orgconfig .revdesc
	@printf "Generating $@\n"
	@$(EMACS_ORG) $< $(ORG_EVAL)

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

%/index.html: %.texi
	@printf "Generating $(PKG)/*.html\n"
	@rm -rf $(PKG)
	@$(MAKEINFO) --html -o $(PKG)/ $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

DOCS_DOMAIN = docs.$(DOMAIN)
STAT_DOMAIN = stats.$(DOMAIN)
SNAP_TARGET = $(subst .,_,$(DOCS_DOMAIN)):devel/$(PKG)/
DOCS_TARGET = $(subst .,_,$(DOCS_DOMAIN)):$(PKG)/
STAT_TARGET = $(subst .,_,$(STAT_DOMAIN)):$(PKG)/

publish: redo-docs
	@printf "Publishing snapshot manual...\n"
	@cp $(PKG).pdf $(PKG)/$(PKG).pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG) $(SNAP_TARGET)

release: redo-docs
	@printf "Publishing release manual...\n"
	@cp $(PKG).pdf $(PKG)/$(PKG).pdf
	@$(RCLONE) sync $(RCLONE_ARGS) $(PKG) $(DOCS_TARGET)

stats:
	@printf "Generating statistics...\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@$(RCLONE) sync $(RCLONE_ARGS) stats $(STAT_TARGET)

CLEAN = .revdesc $(PKG).info dir $(PKG) $(PKG).html $(PKG).pdf $(STAT_TARGET)

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
