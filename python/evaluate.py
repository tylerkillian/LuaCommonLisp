from reader import *
import sys

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

def addition(environment, arguments):
	assert(len(arguments) == 2)
	assert(isSymbol(arguments[0]))
	assert(isSymbol(arguments[1]))
	left = getSymbolValue(arguments[0])
	right = getSymbolValue(arguments[1])
	intResult = int(left) + int(right)
	return Symbol(str(intResult))

def defun(environment, arguments):
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

def createStandardEnvironment():
	return {
		"*standard-output*": sys.stdout,
		"functions": {
			"+": {
				"name": addition,
				"argumentNames": None,
				"body": None,
			}
		},
		"macros": {
			"defun": {
				"name": defun,
				"argumentNames": None,
				"body": None,
			}
		},
	}

def evaluate(environment, expression):
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
				value = evaluate(environment, variableToLookup)
				message = message.replace("~a", str(value))
		environment["*standard-output*"].write(message)
		return None
	elif isSymbol(expression.getCar(), "setf"):
		variable = getSymbolValue(expression.getCdr().getCar())
		value = getSymbolValue(expression.getCdr().getCdr().getCar())
		environment[variable] = value
	elif isSymbol(expression.getCar(), "+"):
		left = expression.getCdr().getCar()
		leftValue = evaluate(environment, left)

		right = expression.getCdr().getCdr().getCar()
		rightValue = evaluate(environment, right)
		return int(leftValue) + int(rightValue)
	elif isSymbol(expression.getCar(), "let"):
		variableToSet = getSymbolValue(expression.getCdr().getCar().getCar().getCar())
		value = getSymbolValue(expression.getCdr().getCar().getCar().getCdr().getCar())
		environment = { "*standard-output*": environment["*standard-output*"] }
		environment[variableToSet] = value
		root = expression.getCdr().getCdr().getCar()
		return evaluate(environment, root)
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
		functionPointer = evaluate(environment, functionName)
		assert((Expression_getLength(expression)-1) == len(functionPointer['arguments']))
		for expressionIndex in range(1, Expression_getLength(expression)):
			argumentName = functionPointer['arguments'][expressionIndex - 1]
			environment[argumentName] = evaluate(environment, Expression_get(expression, expressionIndex))
		returnValue = None
		for command in functionPointer['body']:
			returnValue = evaluate(environment, command)
		
		return returnValue

def isFunction(environment, expression):
	if not isCons(expression):
		return False
	if Expression_getLength(expression) < 1:
		return False
	if not isSymbol(Expression_get(expression, 0)):
		return False

	functionName = getSymbolValue(Expression_get(expression, 0))
	if environment["functions"][functionName]:
		return True

	return False

def evaluate2(environment, expression):
	if isNumber(expression):
		return expression
	elif isSymbol(expression):
		return environment[getSymbolValue(expression)]
	elif isString(expression):
		pass
	elif isFunction(environment, expression):
		functionName = getSymbolValue(Expression_get(expression, 0))
		function = environment["functions"][functionName]["name"]
		argumentsEvaluated = []
		for expressionIndex in range(1, Expression_getLength(expression)):
			nextArgument = Expression_get(expression, expressionIndex)
			argumentsEvaluated.append(evaluate2(environment, nextArgument))
		return function(environment, argumentsEvaluated)
	else:
		assert(isMacro(environment, expression))

