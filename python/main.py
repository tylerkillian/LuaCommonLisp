import sys
from reader import *

def Expression_get(expression, index):
	assert(isCons(expression))
	count = 0
	current = expression
	while count < index:
		assert(isCons(current))
		current = current.getCdr()
		count += 1
	assert(current)
	return current.getCar()

def Expression_getLength(expression):
	assert(isCons(expression))
	length = 0
	current = expression
	while current != NIL:
		current = current.getCdr()
		length += 1
	return length

def eval(expression, environment):
	if isSymbol(expression, "1"):
		return "1"
	elif isSymbol(expression, "2"):
		return "2"
	elif isSymbol(expression, "3"):
		return "3"
	elif isSymbol(expression, "4"):
		return "4"
	elif isSymbol(expression, "5"):
		return "5"
	elif isSymbol(expression):
		return environment[getSymbolValue(expression)]
	elif isSymbol(expression.getCar(), "format"):
		message = getStringValue(expression.getCdr().getCdr().getCar())
		message = message.replace("~%", "\n")
		if expression.getCdr().getCdr().getCdr() != NIL:
			if isCons(expression.getCdr().getCdr().getCdr()):
				variableToLookup = expression.getCdr().getCdr().getCdr().getCar()
				value = eval(variableToLookup, environment)
				message = message.replace("~a", str(value))
		sys.stdout.write(message)
		return None
	elif isSymbol(expression.getCar(), "setf"):
		variable = getSymbolValue(expression.getCdr().getCar())
		value = getSymbolValue(expression.getCdr().getCdr().getCar())
		environment[variable] = value
	elif isSymbol(expression.getCar(), "+"):
		left = expression.getCdr().getCar()
		leftValue = eval(left, environment)

		right = expression.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return int(leftValue) + int(rightValue)
	elif isSymbol(expression.getCar(), "let"):
		variableToSet = getSymbolValue(expression.getCdr().getCar().getCar().getCar())
		value = getSymbolValue(expression.getCdr().getCar().getCar().getCdr().getCar())
		environment = {}
		environment[variableToSet] = value
		root = expression.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif isSymbol(expression.getCar(), "defun"):
		functionName = getSymbolValue(expression.getCdr().getCar())

		argumentsExpression = Expression_get(expression, 2)
		arguments = []
		for expressionIndex in range(0, Expression_getLength(argumentsExpression)):
			arguments.append(getSymbolValue(Expression_get(argumentsExpression, expressionIndex)))
		body = []
		for expressionIndex in range(3, Expression_getLength(expression)):
			body.append(Expression_get(expression, expressionIndex))
		environment[functionName] = {
			"arguments" : arguments,
			"body" : body,
		}
		return environment[functionName]
	else:
		functionName = Expression_get(expression, 0)
		functionPointer = eval(functionName, environment)
		assert((Expression_getLength(expression)-1) == len(functionPointer['arguments']))
		for expressionIndex in range(1, Expression_getLength(expression)):
			argumentName = functionPointer['arguments'][expressionIndex - 1]
			environment[argumentName] = eval(Expression_get(expression, expressionIndex), environment)
		returnValue = None
		for command in functionPointer['body']:
			returnValue = eval(command, environment)
		
		return returnValue

def lisp(inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	environment = {}
	reader = RootReader()
	while nextCharacter:
		result = reader.readNextCharacter(nextCharacter)
		if result:
			reader = RootReader()
			eval(result, environment)
		else:
			nextCharacter = input.read(1)
lisp(sys.argv[1])
