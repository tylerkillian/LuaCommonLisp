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

def eval(expression2, environment):
	if expression2.getType() == "symbol":
		if expression2.getValue() == "1":
			return "1"
		elif expression2.getValue() == "2":
			return "2"
		elif expression2.getValue() == "3":
			return "3"
		elif expression2.getValue() == "4":
			return "4"
		elif expression2.getValue() == "5":
			return "5"
		else:
			return environment[expression2.getValue()]
	elif expression2.getCar().getValue() == "format":
		message = expression2.getCdr().getCdr().getCar().getValue()[1:-1]
		message = message.replace("~%", "\n")
		if expression2.getCdr().getCdr().getCdr():
			if expression2.getCdr().getCdr().getCdr().getType() == "cons":
				variableToLookup = expression2.getCdr().getCdr().getCdr().getCar()
				value = eval(variableToLookup, environment)
				message = message.replace("~a", value)
		sys.stdout.write(message)
		return "nil"
	elif expression2.getCar().getValue() == "setf":
		variable = expression2.getCdr().getCar().getValue()
		value = expression2.getCdr().getCdr().getCar().getValue()
		environment[variable] = value
	elif expression2.getCar().getValue() == "+":
		left = expression2.getCdr().getCar()
		leftValue = eval(left, environment)

		right = expression2.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return str(int(leftValue) + int(rightValue))
	elif expression2.getCar().getValue() == "let":
		variableToSet = expression2.getCdr().getCar().getCar().getCar().getValue()
		value = expression2.getCdr().getCar().getCar().getCdr().getCar().getValue()
		environment = {}
		environment[variableToSet] = value
		root = expression2.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif expression2.getCar().getValue() == "defun":
		functionName = expression2.getCdr().getCar().getValue()
		argument = expression2.getCdr().getCdr().getCar().getCar()
		body = expression2.getCdr().getCdr().getCdr()
		environment[functionName] = {
			"argument" : argument,
			"body" : body,
		}
	else:
		functionName = expression2.getCar()
		functionCode = eval(functionName, environment)

		functionCallArgument = expression2.getCdr().getCar()
		functionCallArgumentEvaluated = Symbol(eval(functionCallArgument, environment))

		cons3 = functionCode['body']

		ae_cons2 = Cons()
		ae_cons2.setCar(functionCallArgumentEvaluated)
		ae_cons2.setCdr(Symbol("nil"))

		ae_cons1 = Cons()
		ae_cons1.setCar(functionCode['argument'])
		ae_cons1.setCdr(ae_cons2)

		argumentsExpression = Cons()
		argumentsExpression.setCar(ae_cons1)
		argumentsExpression.setCdr(Symbol("nil"))

		cons2 = Cons()
		cons2.setCar(argumentsExpression)
		cons2.setCdr(cons3)

		cons1 = Cons()
		cons1.setCar(Symbol("let"))
		cons1.setCdr(cons2)

		letExpression = cons1

		return eval(letExpression, environment)

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
			eval(root.value2, environment)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
