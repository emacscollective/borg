-include ../config.mk
include ../default.mk

docs: info html html-dir pdf

info: $(PKG).info dir
html: $(PKG).html
pdf:  $(PKG).pdf

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH)
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --funcall org-texinfo-export-to-texinfo

# This target first bumps version strings in the Org source.  The
# necessary tools might be missing so other targets do not depend
# on this target and it has to be run explicitly when appropriate.
#
#   AMEND=t make texi    Update manual to be amended to HEAD.
#   VERSION=N make texi  Update manual for release.
#
.PHONY: texi
texi:
	@$(EMACS) $(ORG_ARGS) $(PKG).org $(ORG_EVAL)
	@printf "\n" >> $(PKG).texi
	@rm -f $(PKG).texi~

%.info: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@printf "Generating $@\n"
	@printf "%s" $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

%.html: %.texi
	@printf "Generating $@\n"
	@$(MAKEINFO) --html --no-split $(MANUAL_HTML_ARGS) $<

html-dir: $(PKG).texi
	@printf "Generating $(PKG)/*.html\n"
	@$(MAKEINFO) --html $(MANUAL_HTML_ARGS) $<

%.pdf: %.texi
	@printf "Generating $@\n"
	@texi2pdf --clean $< > /dev/null

.PHONY: stats
stats:
	@printf "Generating statistics\n"
	@gitstats $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

PUBLISH_PATH   ?= /manual/
RELEASE_PATH   ?= /manual/$(VERSION)/
S3_BUCKET      ?= s3://$(DOMAIN)
PUBLISH_TARGET  = $(S3_BUCKET)$(PUBLISH_PATH)
RELEASE_TARGET  = $(S3_BUCKET)$(RELEASE_PATH)
CFRONT_PATHS    = $(PKG).html $(PKG).pdf $(PKG)/*

comma := ,
empty :=
space := $(empty) $(empty)

publish: html html-dir pdf
	@aws s3 cp $(PKG).html $(PUBLISH_TARGET)
	@aws s3 cp $(PKG).pdf  $(PUBLISH_TARGET)
	@aws s3 sync $(PKG)    $(PUBLISH_TARGET)$(PKG)/
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(PUBLISH_PATH),$(CFRONT_PATHS)))" > /dev/null

release: html html-dir pdf
	@aws s3 cp $(PKG).html $(RELEASE_TARGET)
	@aws s3 cp $(PKG).pdf  $(RELEASE_TARGET)
	@aws s3 sync $(PKG)    $(RELEASE_TARGET)$(PKG)/
	@aws s3 cp $(PUBLISH_TARGET)dir.html $(RELEASE_TARGET)dir.html
	@aws s3 cp $(PUBLISH_TARGET)dir/index.html $(RELEASE_TARGET)dir/index.html
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(RELEASE_PATH),$(CFRONT_PATHS)))" > /dev/null

CLEAN = $(PKG).info dir $(PKG) $(PKG).html $(PKG).pdf

clean:
	@printf "Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
