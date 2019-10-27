all: build test

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test
