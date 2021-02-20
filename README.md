# AEIL-haskell-transpiller

Transpiler from AEIL (invented language) to python3 implemented with haskell.
[Check AEIL grammar](https://github.com/AgustinIzaguirre/AEIL-haskell-transpiller/blob/master/grammar.txt).

## Prerequisites
* Stack installed. Check [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* make installed.

## Setup
* Run command `stack setup && stack build`

## Run Transpiller

### Run on Unix based OS
1. Give execution permission to `compiler.sh` with command `chmod u+x compiler.sh`
2. Run command `./compiler.sh PATH_TO_AEIL_FILE`,
    where **PATH_TO_AEIL_FILE** is the path to the *AEIL* program.

After running previous steps, if the *AEIL* program is valid, it will generate the file `output.py` containing transpilation to python3.

### Run on Windows
* Run command `make compile FILE=PATH_TO_AEIL_FILE`, where **PATH_TO_AEIL_FILE** is the path to the *AEIL* program.

After running command, if the *AEIL* program is valid, it will generate the file `output.py` containing transpilation to python3.

### Run using only Stack
* Run command `stack build && stack exec AEIL-transpiler-exe PATH_TO_AEIL_FILE`, where **PATH_TO_AEIL_FILE** is the path to the *AEIL* program.
  
After running command, if the *AEIL* program is valid, it will generate the file `output.py` containing transpilation to python3.

## Run tests

### Run tests with makefile
* Run command `make runTests`
  
### Run tests with stack
* Run command `stack test --silent`
