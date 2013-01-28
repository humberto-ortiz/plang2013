# Posted to PL Online PL 1 course piazza by
# Nate Culwell-Kanare

PSL=../osx-dist/bin/assignment1-osx
TESTS=$(wildcard *.psl)
EXPECTED=$(addsuffix .expected,$(TESTS))

%.psl.expected: %.psl
	$(PSL) --interp < "$<" > "$@" 2> "$(@:.psl.expected=.psl.error)"

.PHONY: tests clean errs

all: tests
tests: last-run
	@touch last-run
	-$(PSL) --test-interps .
last-run: $(EXPECTED)
clean:
	-rm *.psl.error *.psl.expected
errs:
	@-find * -name '*.psl.error' -a -! -empty
