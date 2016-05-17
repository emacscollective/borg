EMACS ?= emacs

all: lisp

lisp: borg.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch -f batch-byte-compile $<

clean:
	@printf "Cleaning\n"
	@rm -f borg-autoloads.el borg.elc borg.info dir
