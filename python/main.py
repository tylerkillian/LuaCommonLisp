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
	elif expression.getChild(0).getChild(0).getName()[7:] == "format":
		message = expression.getChild(0).getChild(1).getChild(1).getChild(0).getName()[8:-1]
		message = message.replace("~%", "\n")
		if expression.getChild(0).getChild(1).getChild(1).getChild(1).getName() == "cons":
			variableToLookup = Node("root")
			variableToLookup.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(1).getChild(0))
			variableToLookup.value2 = expression.value2.getCdr().getCdr().getCdr().getCar()
			value = eval(variableToLookup, environment)
			message = message.replace("~a", value)
		sys.stdout.write(message)
		return "nil"
	elif expression.getChild(0).getChild(0).getName()[7:] == "setf":
		variable = expression.getChild(0).getChild(1).getChild(0).getName()[7:]
		value = expression.getChild(0).getChild(1).getChild(1).getChild(0).getName()[7:]
		environment[variable] = value
	elif expression.getChild(0).getChild(0).getName()[7:] == "+":
		left = Node("root")
		left.addChild(expression.getChild(0).getChild(1).getChild(0))
		left.value2 = expression.value2.getCdr().getCar()
		leftValue = eval(left, environment)

		right = Node("root")
		right.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(0))
		right.value2 = expression.value2.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return str(int(leftValue) + int(rightValue))
	elif expression.getChild(0).getChild(0).getName()[7:] == "let":
		variableToSet = expression.getChild(0).getChild(1).getChild(0).getChild(0).getChild(0).getName()[7:]
		value = expression.getChild(0).getChild(1).getChild(0).getChild(0).getChild(1).getChild(0).getName()[7:]
		environment = {}
		environment[variableToSet] = value
		root = Node("root")
		root.addChild(expression.getChild(0).getChild(1).getChild(1).getChild(0))
		root.value2 = expression.value2.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif expression.getChild(0).getChild(0).getName()[7:] == "defun":
		functionName = expression.getChild(0).getChild(1).getChild(0).getName()[7:]
		argument = expression.getChild(0).getChild(1).getChild(1).getChild(0).getChild(0)
		argument_v2 = expression.value2.getCdr().getCdr().getCar().getCar()
		body = expression.getChild(0).getChild(1).getChild(1).getChild(1)
		body_v2 = expression.value2.getCdr().getCdr().getCdr()
		environment[functionName] = {
			"argument" : argument,
			"argument_v2" : argument_v2,
			"body" : body,
			"body_v2" : body_v2,
		}
	else:
		functionName = Node("root")
		functionName.addChild(expression.getChild(0).getChild(0))
		functionName.value2 = expression.value2.getCar()
		functionCode = eval(functionName, environment)

		functionCallArgument = Node("root")
		functionCallArgument.addChild(expression.getChild(0).getChild(1).getChild(0))
		functionCallArgument.value2 = expression.value2.getCdr().getCar()
		functionCallArgumentEvaluated = Node("symbol_" + eval(functionCallArgument, environment))
		functionCallArgumentEvaluated_v2 = Symbol(eval(functionCallArgument, environment))

		cons3 = functionCode['body']
		cons3_v2 = functionCode['body_v2']

		ae_cons2 = Node("cons")
		ae_cons2.addChild(functionCallArgumentEvaluated)
		ae_cons2.addChild(Node("symbol_nil"))
		ae_cons2_v2 = Cons()
		ae_cons2_v2.setCar(functionCallArgumentEvaluated_v2)
		ae_cons2_v2.setCdr(Symbol("nil"))

		ae_cons1 = Node("cons")
		ae_cons1.addChild(functionCode['argument'])
		ae_cons1.addChild(ae_cons2)
		ae_cons1_v2 = Cons()
		ae_cons1_v2.setCar(functionCode['argument_v2'])
		ae_cons1_v2.setCdr(ae_cons2_v2)

		argumentsExpression = Node("cons")
		argumentsExpression.addChild(ae_cons1)
		argumentsExpression.addChild(Node("symbol_nil"))
		argumentsExpression_v2 = Cons()
		argumentsExpression_v2.setCar(ae_cons1_v2)
		argumentsExpression_v2.setCdr(Symbol("nil"))

		cons2 = Node("cons")
		cons2.addChild(argumentsExpression)
		cons2.addChild(cons3)
		cons2_v2 = Cons()
		cons2_v2.setCar(argumentsExpression_v2)
		cons2_v2.setCdr(cons3_v2)

		cons1 = Node("cons")
		cons1.addChild(Node("symbol_let"))
		cons1.addChild(cons2)
		cons1_v2 = Cons()
		cons1_v2.setCar(Symbol("let"))
		cons1_v2.setCdr(cons2_v2)

		letExpression = Node("root")
		letExpression.addChild(cons1)
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
