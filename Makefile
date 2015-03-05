CASK  ?= cask
EMACS ?= emacs
BATCH  = $(EMACS) --batch -Q

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY: all build

all: build

build: publish.el index.org $(PKGDIR)
	$(CASK) exec $(BATCH) -l publish.el -f org-publish-all

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

.PHONY: clean
clean:
	$(RM) *~
	$(RM) *.elc
