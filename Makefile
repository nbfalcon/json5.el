all: build
build: json5.el
	cask build

test: check
check:
	cask exec ert-runner

.PHONY: check
