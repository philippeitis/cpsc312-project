.PHONY: build
BUILD_DIR=""
build:
	cd utils && make build && cp "$$(stack path --local-install-root)/bin/fast-func-utils-exe" ../fast-func-utils-exe
	swipl main.pl --quit setup
.PHONY: test
test:
	make build
	swipl -g run_tests -t halt test.pl
	swipl -g run_tests -t halt server_test.pl
.PHONY: test-repl
test-repl:
	make build
	swipl test.pl
