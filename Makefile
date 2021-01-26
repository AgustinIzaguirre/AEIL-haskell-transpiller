run:
	stack build && stack exec AEIL-transpiler-exe

compile:
	stack build && stack exec AEIL-transpiler-exe $(FILE)