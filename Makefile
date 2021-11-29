.PHONY: haskell-eval
USER_OS = "$(shell ghci -e 'import System.Info' -e 'putStrLn os')"
FASTFUNC_SERVER_PORT = 4999
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
.PHONY: docker-build
docker-build:
	docker build . -t fast-func-server
.PHONY: docker-test
docker-test:
	make docker-build
	docker run -it fast-func-server /scripts/test.sh
.PHONY: docker-repl
docker-repl:
	make docker-build
	docker run -it fast-func-server /scripts/repl.sh
.PHONY: docker-server
docker-server:
	make docker-build
	@printf "`tput setaf 4`Started server at `tput bold`http://localhost:$(FASTFUNC_SERVER_PORT)\n`tput sgr0`"
	@printf "`tput setaf 4`Go to `tput bold`http://localhost:$(FASTFUNC_SERVER_PORT)/openapi`tput sgr0` for a web based REPL\n"
	docker run -it -p $(FASTFUNC_SERVER_PORT):5000 fast-func-server
.PHONY: docker-start
docker-start:
	make docker-build
	docker run -it fast-func-server /bin/bash
