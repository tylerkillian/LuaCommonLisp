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
	if expression.value2.getType() == "symbol":
		if expression.value2.getValue() == "1":
			return "1"
		elif expression.value2.getValue() == "2":
			return "2"
		elif expression.value2.getValue() == "3":
			return "3"
		elif expression.value2.getValue() == "4":
			return "4"
		elif expression.value2.getValue() == "5":
			return "5"
		else:
			return environment[expression.value2.getValue()]
	elif expression.value2.getCar().getValue() == "format":
		message = expression.value2.getCdr().getCdr().getCar().getValue()[1:-1]
		message = message.replace("~%", "\n")
		if expression.value2.getCdr().getCdr().getCdr():
			if expression.value2.getCdr().getCdr().getCdr().getType() == "cons":
				variableToLookup = Node("root")
				variableToLookup.value2 = expression.value2.getCdr().getCdr().getCdr().getCar()
				value = eval(variableToLookup, environment)
				message = message.replace("~a", value)
		sys.stdout.write(message)
		return "nil"
	elif expression.value2.getCar().getValue() == "setf":
		variable = expression.value2.getCdr().getCar().getValue()
		value = expression.value2.getCdr().getCdr().getCar().getValue()
		environment[variable] = value
	elif expression.value2.getCar().getValue() == "+":
		left = Node("root")
		left.value2 = expression.value2.getCdr().getCar()
		leftValue = eval(left, environment)

		right = Node("root")
		right.value2 = expression.value2.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return str(int(leftValue) + int(rightValue))
	elif expression.value2.getCar().getValue() == "let":
		variableToSet = expression.value2.getCdr().getCar().getCar().getCar().getValue()
		value = expression.value2.getCdr().getCar().getCar().getCdr().getCar().getValue()
		environment = {}
		environment[variableToSet] = value
		root = Node("root")
		root.value2 = expression.value2.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif expression.value2.getCar().getValue() == "defun":
		functionName = expression.value2.getCdr().getCar().getValue()
		argument_v2 = expression.value2.getCdr().getCdr().getCar().getCar()
		body_v2 = expression.value2.getCdr().getCdr().getCdr()
		environment[functionName] = {
			"argument_v2" : argument_v2,
			"body_v2" : body_v2,
		}
	else:
		functionName = Node("root")
		functionName.value2 = expression.value2.getCar()
		functionCode = eval(functionName, environment)

		functionCallArgument = Node("root")
		functionCallArgument.value2 = expression.value2.getCdr().getCar()
		functionCallArgumentEvaluated_v2 = Symbol(eval(functionCallArgument, environment))

		cons3_v2 = functionCode['body_v2']

		ae_cons2_v2 = Cons()
		ae_cons2_v2.setCar(functionCallArgumentEvaluated_v2)
		ae_cons2_v2.setCdr(Symbol("nil"))

		ae_cons1_v2 = Cons()
		ae_cons1_v2.setCar(functionCode['argument_v2'])
		ae_cons1_v2.setCdr(ae_cons2_v2)

		argumentsExpression_v2 = Cons()
		argumentsExpression_v2.setCar(ae_cons1_v2)
		argumentsExpression_v2.setCdr(Symbol("nil"))

		cons2_v2 = Cons()
		cons2_v2.setCar(argumentsExpression_v2)
		cons2_v2.setCdr(cons3_v2)

		cons1_v2 = Cons()
		cons1_v2.setCar(Symbol("let"))
		cons1_v2.setCdr(cons2_v2)

		letExpression = Node("root")
		letExpression.value2 = cons1_v2

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
			eval(root, environment)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
