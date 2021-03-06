Jessica Pan
CMPS 112 Winter 2019
ASG 2 - Interpreter in Ocaml

NAME
   interp.ml - Implementation of interpreter in Ocaml
   interp.mli - Interface of interpreter in Ocaml
   absyn.mli - Definition of the abstract syntax used by interpreter
   parser.mly - Parser reads input into abstract syntax
   scanner.mll - Scans for tokens from the source file and provides lexical specification for the language
   tables.ml - Implementation of Tables module containing the six tables needed by the program, and updated from interp.ml
   tables.mli - Interface of Tables module generated by tables.ml
   main.ml - Performs parsing to create the abstract syntax structure and then calls the interpreter
   etc.ml and etc.mli - Implementation and interface file for Etc module, containing miscellaneous functions

USAGE
   sbinterp [filename.sb]

DESCRIPTION
   The single file name mentioned in argv[1] is read and assumed to be an SB
   program, which is the executed. Each line of the file is scanned by scanner.mll, parsed into abstract syntax by parser.mly, then main.ml parses it int abstract syntax structure.
   The interpreter is then called to read the program into a list. 
   The list is then interpreted by incrementing through each line of the list. 
   The output/results of the interpretation of the file is then printed into STDOUT.
   If there are any errors, error messages are sent to STDERR and the exit code is set to 1.
   
EXIT CODE
0 - program completes successfully
1 - program fails
