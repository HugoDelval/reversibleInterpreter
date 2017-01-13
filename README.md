# Reversible Interpreter

Simple interpreter with debugger and Ctr-Z ability

## Features

 (a) describe how to use the final interpreter

### Run using stack

```bash
git clone https://github.com/HugoDelval/reversibleInterpreter
cd reversibleInterpreter
stack build 
stack exec reversibleInterpreter-exe
```

### Monadic interpreter

The program is able to read from a file given in parameter.

This file should be a Read instance of a *Program*.

You can then execute the loaded program *Statement* by *Statement* and quit at any moment.

Type **1** to go to the next *Statement* and any other key to quit.

### Inspect command

At any moment during the execution of the program, the user can ask to inspect the current state of the variables by typing the **2** key.


 (b) for each of the numbered sections of the project there should be a section of the readme describing how much of the section you have completed.
