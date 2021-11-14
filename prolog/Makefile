.PHONY: test
USER_OS = "$(shell swipl main.pl --quit os)"
FASTFUNC_SERVER_PORT = 9999
ifneq ($(USER_OS), "Windows")
test:
	swipl -g run_tests -t halt test.pl
	python3 server_test.py $(FASTFUNC_SERVER_PORT)
else
test:
	swipl -g run_tests -t halt test.pl
	python server_test.py $(FASTFUNC_SERVER_PORT)
endif

.PHONY: test-repl
test-repl:
	swipl test.pl
