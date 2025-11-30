-include config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          -- Generate lisp and manual)
	$(info make lisp         -- Generate byte-code and autoloads)
	$(info make redo         -- Re-generate byte-code and autoloads)
	$(info make docs         -- Generate all manual formats)
	$(info make redo-docs    -- Re-generate all manual formats)
	$(info make texi         -- Generate texi manual)
	$(info make info         -- Generate info manual)
	$(info make html         -- Generate html manual file)
	$(info make html-dir     -- Generate html manual directory)
	$(info make pdf          -- Generate pdf manual)
	$(info make publish      -- Publish snapshot manuals)
	$(info make release      -- Publish release manuals)
	$(info make stats        -- Generate statistics)
	$(info make stats-upload -- Publish statistics)
	$(info make clean        -- Remove most generated files)
	@printf "\n"

lisp: $(ELCS) autoloads check-declare
redo: clean-lisp lisp

docs:
	@$(MAKE) -C docs docs
redo-docs:
	@$(MAKE) -C docs redo-docs
texi:
	@$(MAKE) -C docs texi
info:
	@$(MAKE) -C docs info
html:
	@$(MAKE) -C docs html
html-dir:
	@$(MAKE) -C docs html-dir
pdf:
	@$(MAKE) -C docs pdf

publish:
	@$(MAKE) -C docs publish
release:
	@$(MAKE) -C docs release

stats:
	@$(MAKE) -C docs stats
stats-upload:
	@$(MAKE) -C docs stats-upload

clean: clean-lisp clean-docs
clean-lisp:
	@printf " Cleaning *...\n"
	@rm -rf $(ELCS) $(PKG)-autoloads.el
clean-docs:
	@$(MAKE) -C docs clean

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
