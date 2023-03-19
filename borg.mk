# Copyright (C) 2016-2023  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# SPDX-License-Identifier: GPL-3.0-or-later

BORG_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

BORG_CLEAN_ELN := true

help::

-include etc/borg/config.mk

ifeq "$(BORG_SECONDARY_P)" "true"
  DRONES_DIR ?= $(shell git config "borg.drones-directory" || echo "elpa")
  BORG_ARGUMENTS = -L $(BORG_DIR) --load borg-elpa \
  --funcall borg-elpa-initialize
else
  DRONES_DIR ?= $(shell git config "borg.drones-directory" || echo "lib")
  BORG_ARGUMENTS = -L $(BORG_DIR) --load borg \
  --funcall borg-initialize
endif

EMACS           ?= emacs
EMACS_ARGUMENTS ?= -Q --batch

EMACS_EXTRA ?=

.PHONY: help helpall clean build native quick-clean quick-build quick \
        init-clean init-build bootstrap bootstrap-borg
.FORCE:

SILENCIO  = --load subr-x
SILENCIO += --eval "(setq byte-compile-warnings '(not docstrings))"
SILENCIO += --eval "(fset 'original-message (symbol-function 'message))"
SILENCIO += --eval "(fset 'message\
(lambda (format &rest args)\
  (unless (or (equal format \"pcase-memoize: equal first branch, yet different\")\
              (equal format \"Not registering prefix \\\"%s\\\" from %s.  Affects: %S\")\
              (and (stringp (car args))\
                   (string-match-p \"Scraping files for\" (car args))))\
    (apply 'original-message format args))))"

## Help

help helpall::
	$(info )
	$(info Getting help)
	$(info ------------)
	$(info make help            = show brief help)
	$(info make helpall         = show extended help)
	$(info )
	$(info Batch targets)
	$(info -------------)
	$(info make clean           = remove all byte-code and native files)
helpall::
	$(info make clean-force     = remove all byte-code files using find)
help helpall::
	$(info make build           = byte-compile all drones and init files)
	$(info make native          = byte+native-compile drones and byte-compile init files)
helpall::
	$(info make quick-clean     = clean most drones and init files)
	$(info make quick-build     = byte-compile most drones and init files)
help helpall::
	$(info make quick           = clean and byte-compile most drones and init files)
	$(info )
	$(info Drone targets)
	$(info -------------)
	$(info make build/DRONE     = byte-compile DRONE)
	$(info make native/DRONE    = byte+native-compile DRONE)
helpall::
	$(info )
	$(info Init file targets)
	$(info -----------------)
	$(info make init-clean      = remove byte-code init files)
	$(info make init-tangle     = recreate init.el from init.org)
	$(info make init-build      = byte-compile init files)
help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap       = bootstrap collective or new drones)
	@printf "\n"

## Batch

clean:
ifeq "$(BORG_CLEAN_ELN)" "true"
	@rm -f init.elc $(INIT_FILES:.el=.elc)
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--funcall borg--batch-clean 2>&1
else
	@find . -name '*.elc' -exec rm '{}' ';'
endif

clean-force:
	@find . -name '*.elc' -exec rm '{}' ';'

build: init-clean
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--funcall borg-batch-rebuild $(INIT_FILES) 2>&1

native: init-clean
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval "(borg-batch-rebuild nil t)" $(INIT_FILES) 2>&1

## Batch Quick

quick-clean: init-clean
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg--batch-clean t)' 2>&1

quick-build:
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-batch-rebuild t)' $(INIT_FILES) 2>&1

quick: quick-clean quick-build

## Per-Clone

clean/%: .FORCE
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-clean "$*")' 2>&1

$(BORG_DIR)borg.mk: ;
build/% $(DRONES_DIR)/% : .FORCE
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-build "$*")' 2>&1

native/%: .FORCE
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-build "$*" nil t)' 2>&1

## Init Files

init-clean:
	@rm -f init.elc $(INIT_FILES:.el=.elc)

init-tangle: init.el
init.el: init.org
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) \
	--load org \
	--eval '(org-babel-tangle-file "init.org")' 2>&1

init-build: init-clean
	@$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) \
	$(BORG_ARGUMENTS) \
	--funcall borg-batch-rebuild-init $(INIT_FILES) 2>&1

## Bootstrap

bootstrap:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	@git submodule init
	@printf "\n=== Running '$(BORG_DIR)borg.sh clone' ===\n"
	@$(BORG_DIR)borg.sh clone
	@printf "\n=== Running '$(BORG_DIR)borg.sh checkout' ===\n"
	@$(BORG_DIR)borg.sh checkout --reset-hard
	@printf "\n=== Running 'make build' ===\n\n"
	@make build
	@printf "\n=== Bootstrapping finished ===\n\n"
	@git submodule status
