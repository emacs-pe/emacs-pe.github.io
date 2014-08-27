CASK  ?= cask
EMACS ?= emacs
BATCH  = $(EMACS) $(EFLAGS) -batch -Q

.PHONY: all
all: elpa
	$(CASK) exec $(BATCH) -l publish.el -f org-publish-all

elpa: Cask
	$(CASK) install
	touch $@

.PHONY: clean
clean:
	rm -f *~
	rm -f *.elc
	rm -f *.html

.PHONY: clean-all
clean-all: clean
	rm -fr elpa .cask
