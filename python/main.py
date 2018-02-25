import sys
from reader import *

def sendToReaderStack(readerStack, nextCharacter):
	characterToProcess = nextCharacter
	while characterToProcess:
		characterToProcess = readerStack[-1].readNextCharacter(readerStack, nextCharacter)
		if len(readerStack) == 0:
			assert(characterToProcess == nextCharacter)
			return characterToProcess

def getValue(environment, value):
	for scope in reversed(environment):
		if scope[value]:
			return scope[value]
	return None

def createParseTree(expression, parent = None):
	if not parent:
		root = Node("root")	
		root.addChild(createParseTree(expression, root))
		return root
	else:
		if type(expression) is str:
			return Node("symbol_" + expression, parent)
		else:
			previousValue = Node("symbol_nil")
			currentCons = Node("cons")
			for value in reversed(expression):
				currentCons.addChild(createParseTree(value, currentCons))
				currentCons.addChild(previousValue)
				previousValue.setParent(currentCons)
				previousValue = currentCons
			topCons = previousValue
			topCons.setParent(parent)
			return topCons

def eval(expression, environment):
	if expression.getChild(0).getName()[0:6] == "symbol":
		return environment[expression.getChild(0).getName()[7:]]
	elif expression.getChild(0).getChild(0).getName()[7:] == "format":
		message = expression.getChild(0).getChild(1).getChild(1).getChild(0).getName()[8:-1]
		message = message.replace("~%", "\n")
		print("a1")
		if expression.getChild(0).getChild(1).getChild(1).getChild(1).getName() == "cons":
			print("a2")
			variableToLookup = Node("root")
			variableToLookup.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(1).getChild(0))
			value = eval(variableToLookup, environment)
			message = message.replace("~a", value)
			print("a3")
		sys.stdout.write(message)
	elif expression.getChild(0).getChild(0).getName()[7:] == "setf":
		variable = expression.getChild(0).getChild(1).getChild(0).getName()[7:]
		value = expression.getChild(0).getChild(1).getChild(1).getChild(0).getName()[7:]
		environment[variable] = value
	elif expression.getChild(0).getChild(0).getName()[7:] == "let":
		variableToSet = expression.getChild(0).getChild(1).getChild(0).getChild(0).getChild(0).getName()[7:]
		value = expression.getChild(0).getChild(1).getChild(0).getChild(0).getChild(1).getChild(0).getName()[7:]
		environment = {}
		environment[variableToSet] = value
		root = Node("root")
		root.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(0))
		eval(root, environment)
	elif expression.getChild(0).getChild(0).getName()[7:] == "defun":
		functionName = expression.getChild(0).getChild(1).getChild(0).getName()[7:]
		argument = expression.getChild(0).getChild(1).getChild(1).getChild(0).getChild(0).getName()[7:]
		print("got defun " + functionName + " " + argument)
		body = Node("root")
		body.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(1))
		environment[functionName] = {
			argument : argument,
			body : body,
		}
	else:
		functionName = Node("root")
		functionName.addChild(expression.getChild(0).getChild(0))
		functionCode = eval(functionName, environment)
		letClause = createParseTree(["let", [["n" "5"]], ["sum", "n"]])
		eval(letClause, environment)
		assert(False)

def lisp(inputFile):
	input = open(inputFile, "r")
	readerStack = []
	nextCharacter = input.read(1)
	environment = {}
	while nextCharacter:
		if len(readerStack) == 0:
			RootReader(readerStack)
			root = readerStack[0].getValue()
		characterToProcess = sendToReaderStack(readerStack, nextCharacter)
		if characterToProcess:
			assert(characterToProcess == nextCharacter)
			eval(root, environment)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
