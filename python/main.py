import sys
from reader import *
from evaluate import evaluate, createStandardEnvironment

def isTestCommand(expression):
	if expressionToString(expression) == "(__run-all-tests__)":
		return True
	else:
		return False

def launchAllTests(environment):
	for functionName in environment["functions"]:
		if functionName[0:5] == "test-":
			print(functionName)
			testExpression = Cons(Symbol(functionName), NIL)
			evaluate(environment, testExpression)

def lisp(mode, inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	environment = createStandardEnvironment()
	reader = RootReader()
	while nextCharacter:
		expression = reader.readNextCharacter(nextCharacter)
		if expression:
			reader = RootReader()

			if isTestCommand(expression):
				launchAllTests(environment)
			else:
				result = evaluate(environment, expression)
				if mode == "normal":
					print(expressionToString(result))
		else:
			nextCharacter = input.read(1)

def parseCommandLineFlags(argv):
    if argv[1] == "-q":
        return "quiet", argv[2]
    else:
        return "normal", argv[1]

mode, filename = parseCommandLineFlags(sys.argv)
lisp(mode, filename)

