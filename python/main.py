import sys
from reader import *
from evaluate import evaluate, createStandardEnvironment

def lisp(inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	environment = createStandardEnvironment()
	reader = RootReader()
	while nextCharacter:
		result = reader.readNextCharacter(nextCharacter)
		if result:
			reader = RootReader()
			evaluate(environment, result)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
