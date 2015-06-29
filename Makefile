# Installation:
#
#     $ bundle install --path vendor/bundle
#     $ bundle exec jekyll serve

CASK   ?= cask
CURL   ?= curl
JEKYLL ?= jekyll

EMACS   = emacs
BATCH   = $(EMACS) --batch -Q

export EMACS

THEME_REV = master
THEME_TGZ = https://github.com/muan/scribble/archive/$(THEME_REV).tar.gz
THEME_TGZ_OPTS = --strip-components 1  \
	--exclude='LICENSE'            \
	--exclude='about.md'           \
	--exclude='README.md'          \
	--exclude='404.html'           \
	--exclude='index.html'         \
	--exclude='Rakefile'           \
	--exclude='Gemfile.lock'       \
	--exclude='.gitignore'         \
	--exclude='_posts'             \
	--exclude='_assets'            \
	--exclude='_config.yml'

serve:
	$(JEKYLL) serve

import-theme:
	$(CURL) -sL "$(THEME_TGZ)" | tar xz $(THEME_TGZ_OPTS)

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
	$(CURL) -sOL $@ "https://github.com/milkypostman/melpa/raw/master/package-build.el"

.INTERMEDIATE: package-build.el
