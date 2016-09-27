Name: Osher Shuman
Student Number: SHMOSH001
Date Due: 22 April 2015
Functional Assignment 1

Files: 
	assignment3.scm - Scheme program that uses many recursive methods to solve a 4*4*4 rubiks cube.
	Questions.pdf - Answers to theory questions in assignment brief.

Note:
The file assignment3.scm was tested using Gambit v4.7.4 interpreter on Windows 8.1.

Unit Testing:
I edited the given automarker and added extra tests. Tests are in the spec.scm file. To run the tests in windows cmd,
navigate to the assignment folder and type "sh mark.sh"

Instructions:
1. Open Gambit interpreter.
2. Type the following where <path> is the path to the file: (load "<path>")
3. Many methods can be called. To solve cube this method must be called -> (solveCube solved initial n)
   Where solved is a list of the solvedStates, initial is the current state and n should be 0.
