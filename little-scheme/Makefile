stack-build = stack build --fast
stack-test = stack test --fast --ghc-options='-Wwarn'
package = little-scheme

build:
	$(stack-build) $(package)

build.watch:
	$(stack-build) --file-watch $(package)

repl:
	stack repl $(package)

test:
	$(stack-test) $(package)

test.watch:
	$(stack-build) --test --file-watch $(package)

clean:
	stack clean
