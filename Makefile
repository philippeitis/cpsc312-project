.PHONY: haskell-eval
USER_OS = "$(shell ghci -e 'import System.Info' -e 'putStrLn os')"
ifneq ($(USER_OS), "mingw32")
haskell-eval:
	cp haskell/stack-unix.yaml haskell/stack.yaml
	cd haskell && make test
haskell-run:
	cp haskell/stack-unix.yaml haskell/stack.yaml
	cd haskell && stack run
endif
haskell-eval:
	copy haskell\stack-win.yaml haskell\stack.yaml
	cd haskell && make test
haskell-run:
	copy haskell\stack-win.yaml haskell\stack.yaml
	cd haskell && stack run
