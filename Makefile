-include config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          - generate lisp and manual)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make docs         - generate most manual formats)
	$(info make texi         - generate texi manual (see comments))
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

docs:
	@$(MAKE) -C docs docs
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
	@$(MAKE) VERSION=$(VERSION) -C docs release

stats:
	@$(MAKE) -C docs stats
stats-upload:
	@$(MAKE) -C docs stats-upload

clean:
	@printf " Cleaning *...\n"
	@rm -rf $(ELCS) $(PKG)-autoloads.el
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
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
            ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))"
