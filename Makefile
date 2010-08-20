# nginxlint make recipe

# settings
HC := ghc

# Don't modify anything below without a reason.
srcs := \
	NginxLint/Data.hs \
	NginxLint/Hint.hs \
	NginxLint/Main.hs \
	NginxLint/Parse.hs

.PHONY: all clean test

all: nginxlint

clean:
	@rm -f nginxlint $(patsubst %.hs,%.hi,${srcs}) $(patsubst %.hs,%.o,${srcs})

nginxlint: ${srcs}
	@$(HC) --make ${srcs} -o $@

test: nginxlint
	@./nginxlint test.conf
