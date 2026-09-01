.PHONY: all build clean test

all: build

build:
	@opam exec -- dune build --profile release
clean:
	@opam exec -- dune clean
