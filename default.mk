TOP := $(dir $(lastword $(MAKEFILE_LIST)))

DOMAIN ?= emacsmirror.org

PKG = borg

ELS   = $(PKG).el
ELS  += $(PKG)-elpa.el
ELCS  = $(ELS:.el=.elc)

# Optional:
DEPS  = cond-let
DEPS += epkg/lisp
DEPS += magit/lisp

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)
REVDESC := $(shell test -e $(TOP).git && git describe --tags)

EMACS      ?= emacs
EMACS_ARGS ?= --eval "(progn \
  (put 'if-let 'byte-obsolete-info nil) \
  (put 'when-let 'byte-obsolete-info nil))"

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH  = -L ../../org/lisp
endif

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref https://$(DOMAIN)/assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://$(DOMAIN)/assets/stats.css -c max_authors=999

RCLONE      ?= rclone
RCLONE_ARGS ?= -v
