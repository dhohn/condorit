.RECIPEPREFIX = >
EMACS ?= emacs
BUILD = build
BATCH = $(EMACS) -Q --batch -L lisp -L test --eval "(setq load-prefer-newer t)" --eval "(package-initialize)"
SRCS  = $(wildcard lisp/condorit*.el)
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile test record clean
all: test

# Compiled files go to $(BUILD)/, not next to the sources: .elc files
# from a newer Emacs cannot be loaded by an older one, and anyone
# loading condorit from this directory should get the sources.
compile:
# Each file in its own Emacs, so a missing require shows up.
> mkdir -p $(BUILD)
> for f in $(SRCS); do \
>   $(BATCH) --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") \"$(BUILD)\")))" -f batch-byte-compile $$f || exit 1; \
> done

test: compile
> $(BATCH) --eval "(push (expand-file-name \"$(BUILD)\") load-path)" -l condorit-test-helper $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

# Record the fixtures of the version tests from htcondor/mini:TAG, e.g.
# make record TAG=25.0.13-el9 (needs apptainer; see test/live/pool).
record:
> test/live/record $(TAG)

clean:
> rm -rf $(BUILD) lisp/*.elc test/*.elc
