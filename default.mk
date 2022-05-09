TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = borg

ELS   = $(PKG).el
ELS  += $(PKG)-elpa.el
ELCS  = $(ELS:.el=.elc)

# Optional:
DEPS  = epkg/lisp
DEPS += magit/lisp

DOMAIN      ?= emacsmirror.net
CFRONT_DIST ?= E1IXJGPIOM4EUW

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999
