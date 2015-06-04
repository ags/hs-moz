PROJECT ?= agsmith/hs-moz
TAG     ?= latest
IMAGE    = $(PROJECT):$(TAG)

.PHONY: build shell repl test

build:
	docker build -t $(IMAGE) .

shell: build
	docker run --rm -it -v $(PWD):/src $(IMAGE) bash

repl: build
	docker run --rm -it $(IMAGE) cabal repl

test: build
	docker run --rm -it $(IMAGE) cabal test
