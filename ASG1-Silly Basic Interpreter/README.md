Jessica Pan
CMPS 112 Winter 2019
ASG 1 - Silly Basic Interpreter

NAME
   sbi.scm - silly basic interpreter

USAGE
   sbi.scm <filename>

DESCRIPTION
   The single file name mentioned in argv[1] is read and assumed to be an SBIR
   program, which is the executed. Each line of the file is read into a list. 
   The list is then interpreted by incrementing through each line of the list. 
   The output/results of the interpretation of the file is then printed into STDOUT.
   If there are any errors, error messages are sent to STDERR and the exit code is set to 1.
   
EXIT CODE
0 - program completes successfully
1 - program fails
