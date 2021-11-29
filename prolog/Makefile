FASTFUNC_SERVER_PORT = 4999
.PHONY: build
build:
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
.PHONY: server
launch-server:
	make build
	swipl main.pl launch $(FASTFUNC_SERVER_PORT)