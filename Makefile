check: dist/setup-config
	cabal build
	dist/build/tokstyle/tokstyle $(shell pkg-config --cflags opus vpx)

dist/setup-config:
	cabal configure
