WGET  ?= wget
CASK  ?= cask
EMACS  = emacs
BATCH  = $(EMACS) --batch -Q

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY: all build

all: build

build: publish.el $(PKGDIR)
	$(CASK) exec $(BATCH) -l publish.el -f org-publish-all

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

.PHONY: clean
clean:
	$(RM) *~
	$(RM) *.elc

.PHONY: build-package
build-package: package-build.el
	$(BATCH) -l $< -f package-build-archive
	$(BATCH) -l $< -f package-build-cleanup

.PHONY: elpa
elpa: package-build.el
	$(BATCH) -l $< -f package-build-all

package-build.el:
	$(WGET) -q -O $@ "https://github.com/milkypostman/melpa/raw/master/package-build.el"

.INTERMEDIATE: package-build.el
