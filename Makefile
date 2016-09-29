.PHONY: calc.js

all: calc.js

calc.js: Main.elm Calc.elm Stack.elm
	elm make --output $@ $^
