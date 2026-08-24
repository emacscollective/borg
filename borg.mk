## Copyright (C) 2016-2026 Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# SPDX-License-Identifier: GPL-3.0-or-later

## Settings

BORG_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

BORG_CLEAN_ELN := true

-include etc/borg/config.mk

ifeq "$(BORG_SECONDARY_P)" "true"
  DRONES_DIR ?= $(shell git config "borg.drones-directory" || echo "elpa")
  BORG_ARGS   = -L $(BORG_DIR) --load borg-elpa \
  --funcall borg-elpa-initialize $(SILENCIO)
else
  DRONES_DIR ?= $(shell git config "borg.drones-directory" || echo "lib")
  BORG_ARGS   = -L $(BORG_DIR) --load borg \
  --funcall borg-initialize $(SILENCIO)
endif

EMACS       ?= emacs
EMACS_ARGS  ?= --eval "(setq load-prefer-newer t)"
EMACS_EXTRA ?=
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(EMACS_EXTRA)

# Initial revisions of these kludges:
# - be368ce85e6 byte-obsolete-generalized-variable
# - a013a0eee79 registering
# - f6e9cb62ea0 succeeded
# - 38e9e01efb1 scraping
SILENCIO += --eval "\
  (define-advice message (:around (fn format &rest args) silencio)\
    (unless (or (member format\
                        '(\"Not registering prefix \\\"%s\\\" from %s.  Affects: %S\"\
                          \"(Shell command succeeded with %s)\"))\
                (and (stringp (car args))\
                     (string-match-p \"Scraping files for\" (car args))))\
      (apply fn format args)))"

.FORCE:
.PHONY: help helpall clean build native quick-clean quick-build quick \
	init-clean init-build bootstrap-borg bootstrap \
	clone-modules checkout-modules

ifeq ($(V),1)
  Q=
else
  Q=@
endif

## Help

help helpall::
	$(info )
	$(info Getting help)
	$(info ------------)
	$(info make help            -- Show brief help)
	$(info make helpall         -- Show extended help)
	$(info )
	$(info Batch targets)
	$(info -------------)
	$(info make clean           -- Remove all byte-code and native files)
helpall::
	$(info make clean-force     -- Remove all byte-code files using find)
help helpall::
	$(info make build           -- Byte-compile all drones and init files)
	$(info make native          -- Byte+native-compile drones and byte-compile init files)
helpall::
	$(info make quick-clean     -- Clean most drones and init files)
	$(info make quick-build     -- Byte-compile most drones and init files)
help helpall::
	$(info make quick           -- Clean and byte-compile most drones and init files)
	$(info make redo            -- Clean and byte-compile all drones and init files)
	$(info )
	$(info Drone targets)
	$(info -------------)
	$(info make build/DRONE     -- Byte-compile DRONE)
	$(info make native/DRONE    -- Byte+native-compile DRONE)
helpall::
	$(info )
	$(info Init file targets)
	$(info -----------------)
	$(info make init-clean      -- Remove byte-code init files)
	$(info make init-tangle     -- Recreate init.el from init.org)
	$(info make init-build      -- Byte-compile init files)
help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap       -- Bootstrap collective or new drones)
	@printf "\n"

## Batch

redo: clean build

clean:
ifeq "$(BORG_CLEAN_ELN)" "true"
	@printf "Cleaning...\n"
	$(Q)rm -f init.elc $(INIT_FILES:.el=.elc)
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--funcall borg--batch-clean 2>&1
	@printf "\n"
else
	$(Q)find . -name '*.elc'          -printf 'removed %p\n' -delete
	$(Q)find . -name '*-autoloads.el' -printf 'removed %p\n' -delete
endif

clean-force:
	$(Q)find . -name '*.elc'          -printf 'removed %p\n' -delete
	$(Q)find . -name '*-autoloads.el' -printf 'removed %p\n' -delete

build: init-clean
	@printf "Building...\n"
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--funcall borg-batch-rebuild $(INIT_FILES) 2>&1

native: init-clean
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--eval "(borg-batch-rebuild nil t)" $(INIT_FILES) 2>&1

## Batch Quick

quick: quick-clean quick-build

quick-clean: init-clean
	@printf "Cleaning...\n"
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--eval '(borg--batch-clean t)' 2>&1
	@printf "\n"

quick-build:
	@printf "Building...\n"
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--eval '(borg-batch-rebuild t)' $(INIT_FILES) 2>&1

## Per-Clone

clean/%: .FORCE
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval '(borg-clean "$*")' 2>&1

# Make tries to rebuild included files.  Since the next rule
# would be used in this case, we need a no-op rule to prevent that.
# https://github.com/magit/magit/issues/3318#issuecomment-357548808
$(DRONES_DIR)/borg/borg.mk: ;

$(DRONES_DIR)/% : .FORCE
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval '(borg-build "$*")' 2>&1

# Define the "aliases" separately to avoid warnings about "peer
# targets" not being updated.
build/%: .FORCE
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval '(borg-build "$*")' 2>&1


native/%: .FORCE
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval '(borg-build "$*" nil t)' 2>&1

## Init Files

init-clean:
	$(Q)rm -f init.elc $(INIT_FILES:.el=.elc)

init-tangle: init.org
	@printf "%s\n" "--- [init.org] ---"
	$(Q)$(EMACS_BATCH) --load org \
	--eval '(org-babel-tangle-file "init.org")' 2>&1

init-build: init-clean
	$(Q)$(EMACS_BATCH) $(subst borg-initialize,ignore,$(BORG_ARGS)) \
	--funcall borg-batch-rebuild-init $(INIT_FILES) 2>&1

ifeq ($(wildcard init.org), init.org)
init-build: init-tangle
endif

## Bootstrap

bootstrap:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	$(Q)git submodule init
	@printf "\n=== Running 'make clone-modules' ===\n"
	$(Q)$(MAKE) clone-modules
	@printf "\n=== Running 'make checkout-modules' ===\n"
	$(Q)$(MAKE) checkout-modules
	@printf "\n=== Running 'make build' ===\n\n"
	$(Q)$(MAKE) build
	@printf "\n=== Bootstrapping finished ===\n\n"
	$(Q)git submodule status

clone-modules:
	$(Q)$(BORG_DIR)borg.sh clone

checkout-modules:
	$(Q)$(BORG_DIR)borg.sh checkout
