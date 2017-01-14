# Reversible Interpreter

Simple interpreter with debugger and Ctr-Z ability

## Features

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

The *Program* samples are in the *progs/* directory. You need to modify the source code to execute another file.

### Inspect command

At any moment during the execution of the program, the user can ask to inspect the current state of the variables by typing the **2** key.

### History of variables

The user can see the history of each variable in the inspect menu (by typing *2* at any moment).

### Step backwards

The user can step backwards into the program by typing the *3* key at any moment. This feature is implemented using a list of executed *Statement*. If the user type *3* then the last Statement is undone and is then push in the *Statements* to be executed.

### Static analysis

TODO
