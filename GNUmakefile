################################################################################
export TMPDIR = $(HOME)/tmp

################################################################################
TMP_DUMMY     = $(TMPDIR)/.dummy
STACK_OPTS    = --stack-yaml=build/stack.yaml

################################################################################
.PHONY: all test clean install

################################################################################
all: $(TMP_DUMMY)
	stack $(STACK_OPTS) setup
	stack $(STACK_OPTS) build
	hlint src

################################################################################
test:
	stack $(STACK_OPTS) test

################################################################################
clean:
	stack $(STACK_OPTS) clean

################################################################################
$(TMP_DUMMY):
	mkdir -p $(dir $@)
	touch $@
