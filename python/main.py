import sys
from reader import *
from evaluate import evaluate

def lisp(inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	environment = {}
	reader = RootReader()
	while nextCharacter:
		result = reader.readNextCharacter(nextCharacter)
		if result:
			reader = RootReader()
			evaluate(result, environment)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
