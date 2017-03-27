
graphdeps:
	scripts/graphdeps.py | dot -Tsvg > dist/deps.svg

so:
	stack build --flag bigchaindb:so
	python scripts/dist-archive.py `ls -t .stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-8.0.1/libHSbigchaindb*.so | head -n 1`


dist-python: so
	cp build/dist-so/*.so frontends/python/bigchaindb_shared/shared.so
	cp -r build/dist-so/libs frontends/python/bigchaindb_shared/
	cp bigchaindb.cabal frontends/python/bigchaindb_shared/
	cd frontends/python && python setup.py sdist
