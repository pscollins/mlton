MLTON := mlton
DEBUG_FLAGS =

.PHONY: unittest
unittest: shallow-flatten-tests pre-flatten-tests

SHALLOW_FLATTEN_TEST_MLBS := $(wildcard shallow-flatten-test-[0-9]*.mlb)
SHALLOW_FLATTEN_TEST_BINS := $(SHALLOW_FLATTEN_TEST_MLBS:.mlb=.bin)

.PHONY: shallow-flatten-tests
shallow-flatten-tests: $(SHALLOW_FLATTEN_TEST_BINS)

shallow-flatten-test-%.bin: shallow-flatten-test-%.mlb shallow-flatten-test-%.sml shallow-flatten-test-common.sml $(wildcard *.fun *.sig)
	$(MLTON) $(DEBUG_FLAGS) -output $@ $<
	./$@

PRE_FLATTEN_TEST_MLBS := $(wildcard pre-flatten-test-[0-9]*.mlb)
PRE_FLATTEN_TEST_BINS := $(PRE_FLATTEN_TEST_MLBS:.mlb=.bin)

.PHONY: pre-flatten-tests
pre-flatten-tests: $(PRE_FLATTEN_TEST_BINS)

pre-flatten-test-%.bin: pre-flatten-test-%.mlb pre-flatten-test-%.sml pre-flatten-test-common.sml $(wildcard *.fun *.sig)
	$(MLTON) $(DEBUG_FLAGS) -output $@ $<
	./$@

.PHONY: clean
clean:
	rm -f *.bin
