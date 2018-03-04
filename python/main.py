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

def expressionToList(expression):
	assert(expression.getType() == "cons")
	current = expression
	result = []
	while current:
		assert(current.getType() == "cons")
		result.append(current.getCar())
		current = current.getCdr()
	return result

def Expression_get(expression, index):
	assert(expression.getType() == "cons")
	count = 0
	current = expression
	while count < index:
		assert(current.getType() == "cons")
		current = current.getCdr()
		count += 1
	assert(current)
	return current.getCar()

def Expression_getLength(expression):
	assert(expression.getType() == "cons")
	length = 0
	current = expression
	while current:
		current = current.getCdr()
		length += 1
	return length

def eval(expression, environment):
	if expression.getType() == "symbol":
		if expression.getValue() == "1":
			return "1"
		elif expression.getValue() == "2":
			return "2"
		elif expression.getValue() == "3":
			return "3"
		elif expression.getValue() == "4":
			return "4"
		elif expression.getValue() == "5":
			return "5"
		else:
			return environment[expression.getValue()]
	elif expression.getCar().getValue() == "format":
		message = expression.getCdr().getCdr().getCar().getValue()[1:-1]
		message = message.replace("~%", "\n")
		if expression.getCdr().getCdr().getCdr():
			if expression.getCdr().getCdr().getCdr().getType() == "cons":
				variableToLookup = expression.getCdr().getCdr().getCdr().getCar()
				value = eval(variableToLookup, environment)
				message = message.replace("~a", value)
		sys.stdout.write(message)
		return "nil"
	elif expression.getCar().getValue() == "setf":
		variable = expression.getCdr().getCar().getValue()
		value = expression.getCdr().getCdr().getCar().getValue()
		environment[variable] = value
	elif expression.getCar().getValue() == "+":
		left = expression.getCdr().getCar()
		leftValue = eval(left, environment)

		right = expression.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return str(int(leftValue) + int(rightValue))
	elif expression.getCar().getValue() == "let":
		variableToSet = expression.getCdr().getCar().getCar().getCar().getValue()
		value = expression.getCdr().getCar().getCar().getCdr().getCar().getValue()
		environment = {}
		environment[variableToSet] = value
		root = expression.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif expression.getCar().getValue() == "defun":
		functionName = expression.getCdr().getCar().getValue()
		argument = expression.getCdr().getCdr().getCar().getCar()
		body = expression.getCdr().getCdr().getCdr()

		argumentsExpression_v2 = Expression_get(expression, 2)
		arguments_v2 = []
		for expressionIndex in range(0, Expression_getLength(argumentsExpression_v2)):
			arguments_v2.append(Expression_get(argumentsExpression_v2, expressionIndex).getValue())
		body_v2 = []
		for expressionIndex in range(3, Expression_getLength(expression)):
			body_v2.append(Expression_get(expression, expressionIndex))
		environment[functionName] = {
			"argument" : argument,
			"body" : body,
			"arguments_v2" : arguments_v2,
			"body_v2" : body_v2,
		}
	else:
		functionName_v2 = Expression_get(expression, 0)
		functionPointer_v2 = eval(functionName_v2, environment)
		assert((Expression_getLength(expression)-1) == len(functionPointer_v2['arguments_v2']))
		for expressionIndex in range(1, Expression_getLength(expression)):
			argumentName = functionPointer_v2['arguments_v2'][expressionIndex - 1]
			environment[argumentName] = eval(Expression_get(expression, expressionIndex), environment)
		returnValue_v2 = None
		for command in functionPointer_v2['body_v2']:
			returnValue_v2 = eval(command, environment)
		
		return returnValue_v2

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
