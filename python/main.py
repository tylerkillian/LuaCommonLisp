import sys
from reader import *

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
	while current != NIL:
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
		if expression.getCdr().getCdr().getCdr() != NIL:
			if expression.getCdr().getCdr().getCdr().getType() == "cons":
				variableToLookup = expression.getCdr().getCdr().getCdr().getCar()
				value = eval(variableToLookup, environment)
				message = message.replace("~a", str(value))
		sys.stdout.write(message)
		return None
	elif expression.getCar().getValue() == "setf":
		variable = expression.getCdr().getCar().getValue()
		value = expression.getCdr().getCdr().getCar().getValue()
		environment[variable] = value
	elif expression.getCar().getValue() == "+":
		left = expression.getCdr().getCar()
		leftValue = eval(left, environment)

		right = expression.getCdr().getCdr().getCar()
		rightValue = eval(right, environment)
		return int(leftValue) + int(rightValue)
	elif expression.getCar().getValue() == "let":
		variableToSet = expression.getCdr().getCar().getCar().getCar().getValue()
		value = expression.getCdr().getCar().getCar().getCdr().getCar().getValue()
		environment = {}
		environment[variableToSet] = value
		root = expression.getCdr().getCdr().getCar()
		return eval(root, environment)
	elif expression.getCar().getValue() == "defun":
		functionName = expression.getCdr().getCar().getValue()

		argumentsExpression = Expression_get(expression, 2)
		arguments = []
		for expressionIndex in range(0, Expression_getLength(argumentsExpression)):
			arguments.append(Expression_get(argumentsExpression, expressionIndex).getValue())
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
