EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

PACKAGE_NAME = tramp-nixshell
PACKAGE_VERSION = 0.1.0
PACKAGE_REQUIRES = "((emacs \"26.1\") (base32 \"0.1\") (transient \"0.4\"))"

ELS = $(PACKAGE_NAME).el
ELCS = $(ELS:.el=.elc)

.PHONY: all clean test

all: $(ELCS)

clean:
	rm -f $(ELCS)
	rm -rf .elpa
	rm -f $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar

%.elc: %.el
	$(BATCH) --eval "(setq package-user-dir (expand-file-name \".elpa\"))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'base32)" \
		--eval "(package-install 'transient)" \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $<

test:
	$(BATCH) --eval "(setq package-user-dir (expand-file-name \".elpa\"))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'base32)" \
		--eval "(package-install 'transient)" \
		-L . \
		-l $(PACKAGE_NAME).el \
		-l test/$(PACKAGE_NAME)-tests.el \
		-f ert-run-tests-batch-and-exit
