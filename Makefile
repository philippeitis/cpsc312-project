.PHONY: haskell-eval
USER_OS = "$(shell ghci -e 'import System.Info' -e 'putStrLn os')"
ifneq ($(USER_OS), "mingw32")
haskell-eval:
	cp haskell/stack-unix.yaml haskell/stack.yaml
	cd haskell && make test
haskell-run:
	cp haskell/stack-unix.yaml haskell/stack.yaml
	cd haskell && stack run
else
haskell-eval:
	copy haskell\stack-win.yaml haskell\stack.yaml
	cd haskell && make test
haskell-run:
	copy haskell\stack-win.yaml haskell\stack.yaml
	cd haskell && stack run
endif
.PHONY: prolog-eval
prolog-eval:
	cd prolog && make test
.PHONY: docker-test
docker-test:
	docker build . -t fast-func-server && docker run -it fast-func-server /bin/bash