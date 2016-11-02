check: dist/setup-config
	cabal build
	dist/build/tokstyle/tokstyle $(shell PKG_CONFIG_PATH=../_install/lib/pkgconfig pkg-config --cflags opus vpx)

dist/setup-config:
	cabal configure
