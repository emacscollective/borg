# Copyright (C) 2016-2019  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# License: GPL v3 <https://www.gnu.org/licenses/gpl-3.0.txt>

-include ../../etc/borg/config.mk

EMACS           ?= emacs
EMACS_ARGUMENTS ?= -Q

.PHONY: all help clean build build-init quick bootstrap
.FORCE:

all: build

SILENCIO  = --load subr-x
SILENCIO += --eval "(put 'if-let   'byte-obsolete-info nil)"
SILENCIO += --eval "(put 'when-let 'byte-obsolete-info nil)"
SILENCIO += --eval "(fset 'original-message (symbol-function 'message))"
SILENCIO += --eval "(fset 'message\
(lambda (format &rest args)\
  (unless (equal format \"pcase-memoize: equal first branch, yet different\")\
    (apply 'original-message format args))))"

help:
	$(info )
	$(info make [all|build]     = rebuild all drones and init files)
	$(info make quick           = rebuild most drones and init files)
	$(info make lib/DRONE       = rebuild DRONE)
	$(info make build-init      = rebuild init files)
	$(info make tangle-init     = recreate init.el from init.org)
	$(info make bootstrap-borg  = bootstrap borg itself)
	$(info make bootstrap       = bootstrap collective or new drones)
	@printf "\n"

clean:
	@find . -name '*.elc' -exec rm '{}' ';'

build:
	@rm -f init.elc
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch -L lib/borg --load borg $(SILENCIO) \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild 2>&1

build-init:
	@rm -f init.elc
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild-init 2>&1

tangle-init: init.el
init.el: init.org
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch --load org \
	--eval '(org-babel-tangle-file "init.org")' 2>&1

quick:
	@rm -f init.elc
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch -L lib/borg --load borg $(SILENCIO) \
	--funcall borg-initialize \
	--eval  '(borg-batch-rebuild t)' 2>&1

lib/borg/borg.mk: ;
lib/%: .FORCE
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch -L lib/borg --load borg $(SILENCIO) \
	--funcall borg-initialize \
	--eval  '(borg-build "$*")' 2>&1

bootstrap:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	@git submodule init
	@printf "\n=== Running 'lib/borg/borg.sh' ===\n"
	@lib/borg/borg.sh
	@printf "\n=== Running 'make build' ===\n\n"
	@make build
