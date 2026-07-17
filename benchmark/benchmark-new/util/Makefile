## Copyright (C) 2013,2019,2021-2022 Matthew Fluet.
# Copyright (C) 2009 Matthew Fluet.
# Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
#    Jagannathan, and Stephen Weeks.
# Copyright (C) 1997-2000 NEC Research Institute.
#
# MLton is released under a HPND-style license.
# See the file MLton-LICENSE for details.
##

ROOT := ../../..
include $(ROOT)/Makefile.config

######################################################################

all: unittest

RUN_MLTON_COMPILE_XARGS := -default-ann 'sequenceNonUnit warn' -default-ann 'warnUnused true'

.PHONY: unittest
unittest: test-util-tests lib-tests
	./test-util-tests
	./lib-tests

test-util-tests: test-util-tests.mlb $(shell $(MLBDEPS) test-util-tests.mlb)
	@echo 'Compiling test-util-tests'
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS) $(RUN_MLTON_COMPILE_XARGS) -target $(TARGET) -output test-util-tests test-util-tests.mlb

lib-tests: lib-tests.mlb $(shell $(MLBDEPS) lib-tests.mlb)
	@echo 'Compiling lib-tests'
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS) $(RUN_MLTON_COMPILE_XARGS) -target $(TARGET) -output lib-tests lib-tests.mlb

CLEAN_EXES := true
$(eval $(MK_COMMON_GOALS))
