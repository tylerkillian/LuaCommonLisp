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
	if expression.getChild(0).getName() == "symbol_5":
		return expression
	elif expression.getChild(0).getName()[0:6] == "symbol":
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
		argument = expression.getChild(0).getChild(1).getChild(1).getChild(0).getChild(0)
		body = expression.getChild(0).getChild(1).getChild(1).getChild(1)
		environment[functionName] = {
			"argument" : argument,
			"body" : body,
		}
	else:
		print("else statement")
		functionName = Node("root")
		functionName.addChild(expression.getChild(0).getChild(0))
		functionCode = eval(functionName, environment)

		functionCallArgument = Node("root")
		print(functionName.getChild(0).getName())
		functionCallArgument.addChild(expression.getChild(0).getChild(1).getChild(0))
		print("evaluating " + expression.getChild(0).getChild(1).getChild(0).getName())
		functionCallArgumentEvaluated = eval(functionCallArgument, environment)
		print("after evaluating")

		cons3 = functionCode['body']

		ae_cons2 = Node("cons")
		ae_cons2.addChild(functionCallArgumentEvaluated)
		ae_cons2.addChild(Node("symbol_nil"))

		ae_cons1 = Node("cons")
		ae_cons1.addChild(functionCode['argument'])
		ae_cons1.addChild(ae_cons2)

		argumentsExpression = Node("cons")
		argumentsExpression.addChild(ae_cons1)
		argumentsExpression.addChild(Node("symbol_nil"))

		cons2 = Node("cons")
		cons2.addChild(argumentsExpression)
		cons2.addChild(cons3)

		cons1 = Node("cons")
		cons1.addChild(Node("symbol_let"))
		cons1.addChild(cons2)

		letExpression = Node("root")
		letExpression.addChild(cons1)

		print(treeToString(letExpression))
		eval(letExpression, environment)

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
