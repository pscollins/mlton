MLTON := mlton
DEBUG_FLAGS =

.PHONY: unittest
unittest: shallow-flatten-tests

SHALLOW_FLATTEN_TEST_MLBS := $(wildcard shallow-flatten-test-[0-9]*.mlb)
SHALLOW_FLATTEN_TEST_BINS := $(SHALLOW_FLATTEN_TEST_MLBS:.mlb=.bin)

.PHONY: shallow-flatten-tests
shallow-flatten-tests: $(SHALLOW_FLATTEN_TEST_BINS)

shallow-flatten-test-%.bin: shallow-flatten-test-%.mlb shallow-flatten-test-%.sml shallow-flatten-test-common.sml $(wildcard *.fun *.sig)
	$(MLTON) $(DEBUG_FLAGS) -output $@ $<
	./$@

.PHONY: clean
clean:
	rm -f *.bin
