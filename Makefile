run:
	stack build && stack exec AEIL-transpiler-exe

compile:
	stack build && stack exec AEIL-transpiler-exe $(FILE)

ghci:
	source /Users/agustin/.ghcup/env && ghci

runTests:
	stack test --silent
