SUBDIRS = src lib

EXTRA_DIST = programs README.md

README: README.md
	pandoc -f markdown -t plain --wrap=none $< -o $@

# Install into the doc directory
doc_DATA = COPYING README

CLEANFILES = README
