from reader import *
import sys

def addition(environment, metadata, arguments):
	assert(len(arguments) == 2)
	assert(isSymbol(arguments[0]))
	assert(isSymbol(arguments[1]))
	left = getSymbolValue(arguments[0])
	right = getSymbolValue(arguments[1])
	intResult = int(left) + int(right)
	return Symbol(str(intResult))

def callUserDefinedFunction(environment, metadata, arguments):
	assert(len(arguments) == len(metadata['argumentNames']))
	for argumentIndex in range(0, len(arguments)):
		argumentName = metadata['argumentNames'][argumentIndex]
		environment[argumentName] = arguments[argumentIndex]
	returnValue = None
	for command in metadata['body']:
		returnValue = evaluate(environment, command)
	return returnValue

def callUserDefinedMacro(environment, metadata, arguments):
	gotRest = False
	rest = None
	for argumentIndex in range(0, len(arguments)):
		if gotRest:
			rest = list_append(rest, arguments[argumentIndex])
		else:
			argumentName = metadata['argumentNames'][argumentIndex]
			if argumentName == "&rest":
				gotRest = True
				restNameIndex = argumentIndex + 1
				rest = list_append(rest, arguments[argumentIndex])
			else:
				environment[argumentName] = arguments[argumentIndex]
	if gotRest:
		restName = metadata['argumentNames'][restNameIndex]
		environment[restName] = rest
	returnValue = None
	for command in metadata['body']:
		returnValue = evaluate(environment, command)
	return returnValue

def isFunction(environment, expression):
	if not isCons(expression):
		return False

def defun(environment, metadata, arguments):
	result = list_new(Symbol(":defun"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def special_defun(environment, metadata, argumentsToDefun):
	functionName = getSymbolValue(argumentsToDefun[0])
	argumentNames = []
	for argumentIndex in range(0, list_getLength(argumentsToDefun[1])):
		argumentNames.append(getSymbolValue(list_get(argumentsToDefun[1], argumentIndex)))
	body = []
	for argumentIndex in range(2, len(argumentsToDefun)):
		body.append(argumentsToDefun[argumentIndex])
	environment["functions"][functionName] = {
		"name": callUserDefinedFunction, 
		"argumentNames": argumentNames,
		"body": body,
	}
	return

def defmacro(environment, metadata, arguments):
	result = list_new(Symbol(":defmacro"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def special_defmacro(environment, metadata, argumentsToDefmacro):
	macroName = getSymbolValue(argumentsToDefmacro[0])
	argumentNames = []
	for argumentIndex in range(0, list_getLength(argumentsToDefmacro[1])):
		argumentNames.append(getSymbolValue(list_get(argumentsToDefmacro[1], argumentIndex)))
	body = []
	for argumentIndex in range(2, len(argumentsToDefmacro)):
		body.append(argumentsToDefmacro[argumentIndex])
	environment["macros"][macroName] = {
		"name": callUserDefinedMacro, 
		"argumentNames": argumentNames,
		"body": body,
	}
	return

def setf(environment, metadata, arguments):
	result = list_new(Symbol(":setf"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def special_setf(environment, metadata, arguments):
	environment[getSymbolValue(arguments[0])] = evaluate(environment, arguments[1])
	return

def special_quote(environment, metadata, arguments):
	return arguments[0]

def fn_list(environment, metadata, arguments):
	result = None
	for argument in arguments:
		result = list_append(result, argument)
	return result

def fn_append(environment, metadata, arguments):
	result = None
	for nextList in arguments:
		for listIndex in range(0, list_getLength(nextList)):
			nextElement = list_get(nextList, listIndex)
			result = list_append(result, nextElement)
	return result

def fn_consp(environment, metadata, arguments):
	if isCons(arguments[0]):
		return Symbol("t")
	else:
		return NIL

def format(environment, metadata, arguments):
	message = getStringValue(arguments[1])
	message = message.replace("~%", "\n")
	if len(arguments) > 2:
		variableToLookup = arguments[2]
		value = evaluate(environment, variableToLookup)
		message = message.replace("~a", getSymbolValue(value))
	environment["*standard-output*"].write(message)
	return NIL

def let(environment, metadata, arguments):
	variableToSet = getSymbolValue(arguments[0].getCar().getCar())
	value = arguments[0].getCar().getCdr().getCar()
	localEnvironment = copyEnvironment(environment)
	localEnvironment[variableToSet] = value
	body = arguments[1]
	return evaluate(localEnvironment, body)

def isTrue(value):
	if value == NIL:
		return False

	if getSymbolValue(value) == "t":
		return True
	else:
		return False

def cl_if(environment, metadata, arguments):
	condition = arguments[0]
	callIfTrue = arguments[1]
	callIfFalse = arguments[2]

	conditionEvaluated = evaluate(environment, condition)
	if isTrue(conditionEvaluated):
		return evaluate(environment, callIfTrue)
	else:
		return evaluate(environment, callIfFalse)

def progn(environment, metadata, arguments):
	for nextExpression in arguments:
		lastReturnValue = evaluate(environment, nextExpression)
	return lastReturnValue

def createStandardEnvironment():
	return {
		"*standard-output*": sys.stdout,
		"t": Symbol("t"),
		"nil": NIL,
		"functions": {
			"+": {
				"name": addition,
				"argumentNames": None,
				"body": None,
			},
			"consp": {
				"name": fn_consp,
				"argumentNames": None,
				"body": None,
			},
			"format": {
				"name": format,
				"argumentNames": None,
				"body": None,
			},
			"list": {
				"name": fn_list,
				"argumentNames": None,
				"body": None,
			},
			"append": {
				"name": fn_append,
				"argumentNames": None,
				"body": None,
			},
		},
		"macros": {
			"defun": {
				"name": defun,
				"argumentNames": None,
				"body": None,
			},
			"defmacro": {
				"name": defmacro,
				"argumentNames": None,
				"body": None,
			},
			"setf": {
				"name": setf,
				"argumentNames": None,
				"body": None,
			},
		},
		"special": {
			":defun": {
				"name": special_defun,
				"argumentNames": None,
				"body": None,
			},
			":defmacro": {
				"name": special_defmacro,
				"argumentNames": None,
				"body": None,
			},
			":setf": {
				"name": special_setf,
				"argumentNames": None,
				"body": None,
			},
			"let": {
				"name": let,
				"argumentNames": None,
				"body": None,
			},
			"if": {
				"name": cl_if,
				"argumentNames": None,
				"body": None,
			},
			"quote": {
				"name": special_quote,
				"argumentNames": None,
				"body": None,
			},
			"progn": {
				"name": progn,
				"argumentNames": None,
				"body": None,
			},
		},
	}

def copyEnvironment(environment):
	copy =  {}
	for key in environment:
		copy[key] = environment[key]
	return copy

def isFunction(environment, expression):
	if not isCons(expression):
		return False
	if list_getLength(expression) < 1:
		return False
	if not isSymbol(list_get(expression, 0)):
		return False

	functionName = getSymbolValue(list_get(expression, 0))
	if functionName in environment["functions"]:
		return True

	return False

def isMacro(environment, expression):
	if not isCons(expression):
		return False
	if list_getLength(expression) < 1:
		return False
	if not isSymbol(list_get(expression, 0)):
		return False

	macroName = getSymbolValue(list_get(expression, 0))
	if macroName in environment["macros"]:
		return True

	return False

def isSpecial(environment, expression):
	if not isCons(expression):
		return False
	if list_getLength(expression) < 1:
		return False
	if not isSymbol(list_get(expression, 0)):
		return False

	macroName = getSymbolValue(list_get(expression, 0))
	if macroName in environment["special"]:
		return True

	return False

def evaluate(environment, expression):
	expression = expandBackquoteMacro(expression)

	if isNumber(expression):
		return expression
	elif isSymbol(expression):
		return environment[getSymbolValue(expression)]
	elif isString(expression):
		return expression
	elif isFunction(environment, expression):
		functionName = getSymbolValue(list_get(expression, 0))
		function = environment["functions"][functionName]["name"]
		argumentsEvaluated = []
		for expressionIndex in range(1, list_getLength(expression)):
			nextArgument = list_get(expression, expressionIndex)
			argumentsEvaluated.append(evaluate(environment, nextArgument))
		metadata = environment["functions"][functionName]
		return function(environment, metadata, argumentsEvaluated)
	elif isMacro(environment, expression):
		macroName = getSymbolValue(list_get(expression, 0))
		macro = environment["macros"][macroName]["name"]
		arguments = []
		for expressionIndex in range(1, list_getLength(expression)):
			arguments.append(list_get(expression, expressionIndex))
		metadata = environment["macros"][macroName]
		return evaluate(environment, macro(environment, metadata, arguments))
	elif isSpecial(environment, expression):
		assert(isSpecial(environment, expression))
		operatorName = getSymbolValue(list_get(expression, 0))
		operator = environment["special"][operatorName]["name"]
		arguments = []
		for expressionIndex in range(1, list_getLength(expression)):
			arguments.append(list_get(expression, expressionIndex))
		metadata = environment["special"][operatorName]
		return operator(environment, metadata, arguments)
	else:
		print("failed on " + expressionToString(expression))
		assert(False)

