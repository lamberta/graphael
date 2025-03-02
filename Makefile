# Use environment variable EMACS if set, otherwise use emacs in PATH
EMACS ?= emacs

LIB_FILES = -l graphael-core.el \
            -l graphael-operations.el \
            -l graphael-algorithms.el \
            -l graphael.el

TEST_FILES = -l graphael-core-test.el \
             -l graphael-operations-test.el \
             -l graphael-algorithms-test.el \
             -l graphael-boundary-test.el

all:
	$(EMACS) --quick --batch \
		$(LIB_FILES) \
		$(TEST_FILES) \
		-f batch-byte-compile *.el

test:
	$(EMACS) --quick --batch \
		-l ert \
		$(LIB_FILES) \
		$(TEST_FILES) \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

.PHONY: all test clean
