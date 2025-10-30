-include config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          - generate lisp and manual)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make redo         - re-generate byte-code and autoloads)
	$(info make docs         - generate all manual formats)
	$(info make redo-docs    - re-generate all manual formats)
	$(info make texi         - generate texi manual)
	$(info make info         - generate info manual)
	$(info make html         - generate html manual file)
	$(info make html-dir     - generate html manual directory)
	$(info make pdf          - generate pdf manual)
	$(info make publish      - publish snapshot manuals)
	$(info make release      - publish release manuals)
	$(info make stats        - generate statistics)
	$(info make stats-upload - publish statistics)
	$(info make clean        - remove most generated files)
	@printf "\n"

lisp: $(ELCS) loaddefs check-declare
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

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
