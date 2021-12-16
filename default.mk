TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = borg

ELS   = $(PKG).el
ELS  += $(PKG)-elpa.el
ELCS  = $(ELS:.el=.elc)

DEPS  =

DOMAIN      ?= emacsmirror.net
CFRONT_DIST ?= E1IXJGPIOM4EUW

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
ORG_LOAD_PATH += -L ../../ox-texinfo+
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

STATS_DIR ?= $(TOP)docs/stats
