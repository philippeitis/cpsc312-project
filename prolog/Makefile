.PHONY: test
USER_OS = "$(shell swipl main.pl --quit os)"
ifneq ($(USER_OS), "Windows")
test:
	swipl -g run_tests -t halt test.pl
	python3 server_test.py
else
test:
	swipl -g run_tests -t halt test.pl
	python server_test.py
endif

.PHONY: test-repl
test-repl:
	swipl test.pl
