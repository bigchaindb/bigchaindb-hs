
graphdeps:
	scripts/graphdeps.py | dot -Tsvg > dist/deps.svg

build-so:
	stack setup
	stack build --flag bigchaindb:so --ghc-options="-lHSrts-ghc8.0.1" -v

so: build-so
	python3 scripts/dist-archive.py `ls -t .stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-8.0.1/libHSbigchaindb*.so | head -n 1`

so-alt:
	python3 scripts/dist-archive.py `ls -t .stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-8.0.1/bigchaindb*/libHSbigchaindb*.so | head -n 1`

clean:
	rm -rf build

dist-python: clean so
	cp build/dist-so/*.so frontends/python/bigchaindb_shared/shared.so
	cp -r build/dist-so/libs frontends/python/bigchaindb_shared/
	cp bigchaindb.cabal frontends/python/bigchaindb_shared/
	cd frontends/python && python setup.py sdist
