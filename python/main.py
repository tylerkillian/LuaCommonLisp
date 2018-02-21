import sys
from reader import *

def sendToReaderStack(readerStack, nextCharacter):
	characterToProcess = nextCharacter
	while characterToProcess:
		characterToProcess = readerStack[-1].readNextCharacter(readerStack, nextCharacter)
		if len(readerStack) == 0:
			assert(characterToProcess == nextCharacter)
			return characterToProcess

def eval(expression):
	if expression.getChild(0).getChild(0).getName()[7:] == "format":
		message = expression.getChild(0).getChild(1).getChild(1).getChild(0).getName()[8:-1]
		print(message.replace("~%", "\n"))
	elif expression.getChild(0).getChild(0).getName()[7:] == "setf":
	else:
		assert(False)

def lisp(inputFile):
	input = open(inputFile, "r")
	readerStack = []
	nextCharacter = input.read(1)
	while nextCharacter:
		if len(readerStack) == 0:
			RootReader(readerStack)
			root = readerStack[0].getValue()
		characterToProcess = sendToReaderStack(readerStack, nextCharacter)
		if characterToProcess:
			assert(characterToProcess == nextCharacter)
			eval(root)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
