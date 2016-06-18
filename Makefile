.PHONY: calc.js

all: calc.js

calc.js: Calc.elm Stack.elm
	elm make --output $@ $^
