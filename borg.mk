# Copyright (C) 2016-2024 Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# SPDX-License-Identifier: GPL-3.0-or-later

BORG_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

BORG_CLEAN_ELN := true

help::

-include etc/borg/config.mk

ifeq ($(V),1)
  Q=
else
  Q=@
endif

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

SILENCIO += --eval "(progn (require 'gv) (put 'buffer-substring 'byte-obsolete-generalized-variable nil))"
SILENCIO += --eval "(define-advice message (:around (fn format &rest args) silencio)\
  (unless (or (equal format \"Not registering prefix \\\"%s\\\" from %s.  Affects: %S\")\
              (ignore-errors (string-match-p \"Scraping files for\" (car args))))\
    (apply fn format args)))"

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
	$(Q)rm -f init.elc $(INIT_FILES:.el=.elc)
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--funcall borg--batch-clean 2>&1
else
	$(Q)find . -name '*.elc'          -printf 'removed %p\n' -delete
	$(Q)find . -name '*-autoloads.el' -printf 'removed %p\n' -delete
endif

clean-force:
	$(Q)find . -name '*.elc'          -printf 'removed %p\n' -delete
	$(Q)find . -name '*-autoloads.el' -printf 'removed %p\n' -delete

build: init-clean
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--funcall borg-batch-rebuild $(INIT_FILES) 2>&1

native: init-clean
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval "(borg-batch-rebuild nil t)" $(INIT_FILES) 2>&1

## Batch Quick

quick-clean: init-clean
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg--batch-clean t)' 2>&1

quick-build:
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-batch-rebuild t)' $(INIT_FILES) 2>&1

quick: quick-clean quick-build

## Per-Clone

clean/%: .FORCE
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-clean "$*")' 2>&1

$(BORG_DIR)borg.mk: ;
build/% $(DRONES_DIR)/% : .FORCE
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-build "$*")' 2>&1

native/%: .FORCE
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) $(SILENCIO) \
	$(BORG_ARGUMENTS) \
	--eval '(borg-build "$*" nil t)' 2>&1

## Init Files

init-clean:
	$(Q)rm -f init.elc $(INIT_FILES:.el=.elc)

init-tangle: init.org
	@printf "%s\n" "--- [init.org] ---"
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) \
	--load org \
	--eval '(org-babel-tangle-file "init.org")' 2>&1

init-build: init-clean
	$(Q)$(EMACS) $(EMACS_ARGUMENTS) $(EMACS_EXTRA) \
	$(BORG_ARGUMENTS) \
	--funcall borg-batch-rebuild-init $(INIT_FILES) 2>&1

ifeq ($(wildcard init.org), init.org)
init-build: init-tangle
endif

## Bootstrap

bootstrap:
	$(Q)printf "\n=== Running 'git submodule init' ===\n\n"
	$(Q)git submodule init
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh clone' ===\n"
	$(Q)$(BORG_DIR)borg.sh clone
	$(Q)printf "\n=== Running '$(BORG_DIR)borg.sh checkout' ===\n"
	$(Q)$(BORG_DIR)borg.sh checkout --reset-hard
	$(Q)printf "\n=== Running 'make build' ===\n\n"
	$(Q)$(MAKE) build
	$(Q)printf "\n=== Bootstrapping finished ===\n\n"
	$(Q)git submodule status
