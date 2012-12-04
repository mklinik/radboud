ArgEnv
Version  1.0.3
Ronny Wichers Schreur
ronny@cs.kun.nl

The ArgEnv package provides a Clean interface to the command line
arguments and the environment variables. 

This is the README for the Windows version. The Windows version has
been tested on Windows XP with Clean 2.1, but it should work with
all Windows versions with Clean 2.0 or better.


FILES

    README.txt
        This file
    CHANGES.txt
        Changes history
    ArgEnv.dcl
        Definition of the interface
    ArgEnv.icl
        Implementation of the interface
    ArgEnvC.c
        Implementation of the C side of the interface
    Clean System Files/ArgEnvC.obj
        Object file from ArgEnvC.c
    printenv.icl
        An example program that prints the value of an environment
        variable
    printenv.prj
        Project file for the example program

USAGE

    - add the ArgEnv directory to the search paths of your project
    - import ArgEnv

BUGS

There is no way to stop the Clean run-time system from interpreting
some of the command-line arguments.

If you start a Clean program from the command-line prompt, you still
have to "press any key" before the program quits.
