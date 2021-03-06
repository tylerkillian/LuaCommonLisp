from reader import *
import sys

def isTrue(value):
	if value == NIL:
		return False
	else:
		return True

def function_addition(environment, metadata, arguments):
	result = 0
	for nextArgument in arguments:
		assert(isSymbol(nextArgument))
		nextValue = getSymbolValue(nextArgument)
		result += int(nextValue)
	return Symbol(str(result))

def function_append(environment, metadata, arguments):
	result = None
	for nextList in arguments:
		for listIndex in range(0, list_getLength(nextList)):
			nextElement = list_get(nextList, listIndex)
			result = list_append(result, nextElement)
	return result

def function_apply(environment, metadata, arguments):
	assert(isFunctionPointer(arguments[0]))
	assert(isCons(arguments[len(arguments)-1]))
	argumentsToAppliedFunction = []
	for argumentIndex in range(1, len(arguments)-1):
		argumentsToAppliedFunction.append(arguments[argumentIndex])
	for argumentIndex in range(0, list_getLength(arguments[len(arguments)-1])):
		argumentsToAppliedFunction.append(list_get(arguments[len(arguments)-1], argumentIndex))
	functionPointer = arguments[0]
	metadata = functionPointer.getExtra()
	function = metadata["name"]
	return function(environment, metadata, argumentsToAppliedFunction)

def function_car(environment, metadata, arguments):
	if arguments[0] == NIL:
		return NIL

	assert(isCons(arguments[0]))
	return arguments[0].getCar()

def function_cdr(environment, metadata, arguments):
	if arguments[0] == NIL:
		return NIL

	assert(isCons(arguments[0]))
	return arguments[0].getCdr()

def function_cons(environment, metadata, arguments):
	assert(len(arguments) == 2)
	return Cons(arguments[0], arguments[1])

def function_consp(environment, metadata, arguments):
	if isCons(arguments[0]):
		return Symbol("t")
	else:
		return NIL

def function_eql(environment, metadata, arguments):
	if arguments[0] == arguments[1]:
		return Symbol("t")
	elif isSymbol(arguments[0]) and isSymbol(arguments[1]):
		if getSymbolValue(arguments[0]) == getSymbolValue(arguments[1]):
			return Symbol("t")
		else:
			return NIL
	else:
		return NIL

def consAreEqual(a, b):
	if a == NIL:
		if b == NIL:
			return True
		else:
			return False
	elif isCons(a):
		if not isCons(b):
			return False
		else:
			return consAreEqual(a.getCar(), b.getCar()) and consAreEqual(a.getCdr(), b.getCdr())
	else:
		assert(isSymbol(a))
		if not isSymbol(b):
			return False
		elif getSymbolValue(a) != getSymbolValue(b):
			return False
		else:
			return True

def function_equal(environment, metadata, arguments):
	assert(len(arguments) == 2)
	if consAreEqual(arguments[0], arguments[1]):
		return Symbol("t")
	else:
		return NIL

def function_format(environment, metadata, arguments):
	message = getStringValue(arguments[1])
	message = message.replace("~%", "\n")
	if len(arguments) > 2:
		variableToLookup = arguments[2]
		value = evaluate(environment, variableToLookup)
		message = message.replace("~a", getSymbolValue(value))
	environment["*standard-output*"].write(message)
	return NIL

def function_greaterThan(environment, metadata, arguments):
	assert(len(arguments) == 2)
	left = int(getSymbolValue(arguments[0]))
	right = int(getSymbolValue(arguments[1]))
	if left > right:
		return Symbol("t")
	else:
		return NIL

def function_list(environment, metadata, arguments):
	result = None
	for argument in arguments:
		result = list_append(result, argument)
	return result

def function_null(environment, metadata, arguments):
	if arguments[0] == NIL:
		return Symbol("t")
	else:
		return NIL

def function_subtraction(environment, metadata, arguments):
	assert(len(arguments) >= 1)
	result = int(getSymbolValue(arguments[0]))
	for argumentIndex in range(1, len(arguments)):
		nextArgument = arguments[argumentIndex]
		assert(isSymbol(nextArgument))
		nextValue = getSymbolValue(nextArgument)
		result -= int(nextValue)
	return Symbol(str(result))

def function_zerop(environment, metadata, arguments):
	if isSymbol(arguments[0], "0"):
		return Symbol("t")
	else:
		return NIL

def callUserDefinedFunction(environment, metadata, arguments):
	gotRest = False
	rest = None
	newEnvironment = copyEnvironment(environment)
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
				newEnvironment[argumentName] = arguments[argumentIndex]
	if gotRest:
		restName = metadata['argumentNames'][restNameIndex]
		newEnvironment[restName] = rest
	returnValue = None
	for command in metadata['body']:
		returnValue = evaluate(newEnvironment, command)
	return returnValue


def macro_assert(environment, metadata, arguments):
	assert(len(arguments) == 1)
	condition = arguments[0]

	conditionEvaluated = evaluate(environment, condition)
	assert(isTrue(conditionEvaluated))
	return NIL

def macro_defmacro(environment, metadata, arguments):
	result = list_new(Symbol(":defmacro"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def macro_backquote(environment, metadata, arguments):
	originalExpression = list_new(Symbol("backquote"))
	for argument in arguments:
		originalExpression = list_append(originalExpression, argument)
	return expandBackquoteMacro(originalExpression)

def macro_defun(environment, metadata, arguments):
	result = list_new(Symbol(":defun"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def macro_lambda(environment, metadata, arguments):
	result = list_new(Symbol("function"))
	lambdaExpression = list_new(Symbol("lambda"))
	for argument in arguments:
		lambdaExpression = list_append(lambdaExpression, argument)
	return list_append(result, lambdaExpression)

def macro_setf(environment, metadata, arguments):
	result = list_new(Symbol(":setf"))
	for argument in arguments:
		result = list_append(result, argument)
	return result

def callUserDefinedMacro(environment, metadata, arguments):
	gotRest = False
	rest = None
	newEnvironment = copyEnvironment(environment)
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
				newEnvironment[argumentName] = arguments[argumentIndex]
	if gotRest:
		restName = metadata['argumentNames'][restNameIndex]
		newEnvironment[restName] = rest
	returnValue = None
	for command in metadata['body']:
		returnValue = evaluate(newEnvironment, command)
	return returnValue

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
	return argumentsToDefun[0]

def special_function(environment, metadata, arguments):
	if isSymbol(arguments[0]):
		functionName = getSymbolValue(arguments[0])
		assert(environment["functions"][functionName])
		return FunctionPointer(functionName, environment["functions"][functionName])
	else:
		assert(isCons(arguments[0]))
		assert(isSymbol(list_get(arguments[0], 0), "lambda"))

		lambdaForm = arguments[0]
		lambdaArgumentNames = list_get(lambdaForm, 1) 

		functionPointerArgumentNames = []
		for lambdaArgumentIndex in range(0, list_getLength(lambdaArgumentNames)):
			functionPointerArgumentNames.append(getSymbolValue(list_get(lambdaArgumentNames, lambdaArgumentIndex)))

		functionPointerBody = []
		for lambdaBodyIndex in range(2, list_getLength(lambdaForm)):
			functionPointerBody.append(list_get(lambdaForm, lambdaBodyIndex))

		return FunctionPointer("lambda", {
			"name": callUserDefinedFunction, 
			"argumentNames": functionPointerArgumentNames,
			"body": functionPointerBody,
		})

def special_if(environment, metadata, arguments):
	condition = arguments[0]
	callIfTrue = arguments[1]
	callIfFalse = arguments[2]

	conditionEvaluated = evaluate(environment, condition)
	if isTrue(conditionEvaluated):
		return evaluate(environment, callIfTrue)
	else:
		return evaluate(environment, callIfFalse)

def special_let(environment, metadata, arguments):
	assert(len(arguments) >= 2)
	localEnvironment = copyEnvironment(environment)
	for variableIdx in range(0, list_getLength(arguments[0])):
		variableDefinition = list_get(arguments[0], variableIdx)
		variableToSet = getSymbolValue(list_get(variableDefinition, 0))
		variableValue = evaluate(environment, list_get(variableDefinition, 1))
		localEnvironment[variableToSet] = variableValue
	for expressionIdx in range(1, len(arguments)):
		nextExpression = arguments[expressionIdx]
		lastReturnValue = evaluate(localEnvironment, nextExpression)
	return lastReturnValue

def special_progn(environment, metadata, arguments):
	for nextExpression in arguments:
		lastReturnValue = evaluate(environment, nextExpression)
	return lastReturnValue

def special_quote(environment, metadata, arguments):
	return arguments[0]

def special_setf(environment, metadata, arguments):
	environment[getSymbolValue(arguments[0])] = evaluate(environment, arguments[1])
	return

def createStandardEnvironment():
	environment = {
		"*standard-output*": sys.stdout,
		"t": Symbol("t"),
		"nil": NIL,
		"functions": {
			"+": {
				"name": function_addition,
				"argumentNames": None,
				"body": None,
			},
			"-": {
				"name": function_subtraction,
				"argumentNames": None,
				"body": None,
			},
			">": {
				"name": function_greaterThan,
				"argumentNames": None,
				"body": None,
			},
			"append": {
				"name": function_append,
				"argumentNames": None,
				"body": None,
			},
			"apply": {
				"name": function_apply,
				"argumentNames": None,
				"body": None,
			},
			"car": {
				"name": function_car,
				"argumentNames": None,
				"body": None,
			},
			"cdr": {
				"name": function_cdr,
				"argumentNames": None,
				"body": None,
			},
			"cons": {
				"name": function_cons,
				"argumentNames": None,
				"body": None,
			},
			"consp": {
				"name": function_consp,
				"argumentNames": None,
				"body": None,
			},
			"eql": {
				"name": function_eql,
				"argumentNames": None,
				"body": None,
			},
			"equal": {
				"name": function_equal,
				"argumentNames": None,
				"body": None,
			},
			"format": {
				"name": function_format,
				"argumentNames": None,
				"body": None,
			},
			"list": {
				"name": function_list,
				"argumentNames": None,
				"body": None,
			},
			"null": {
				"name": function_null,
				"argumentNames": None,
				"body": None,
			},
			"zerop": {
				"name": function_zerop,
				"argumentNames": None,
				"body": None,
			},
		},
		"macros": {
			"assert": {
				"name": macro_assert,
				"argumentNames": None,
				"body": None,
			},
			"backquote": {
				"name": macro_backquote,
				"argumentNames": None,
				"body": None,
			},
			"defun": {
				"name": macro_defun,
				"argumentNames": None,
				"body": None,
			},
			"defmacro": {
				"name": macro_defmacro,
				"argumentNames": None,
				"body": None,
			},
			"lambda": {
				"name": macro_lambda,
				"argumentNames": None,
				"body": None,
			},
			"setf": {
				"name": macro_setf,
				"argumentNames": None,
				"body": None,
			},
		},
		"special": {
			":defmacro": {
				"name": special_defmacro,
				"argumentNames": None,
				"body": None,
			},
			":defun": {
				"name": special_defun,
				"argumentNames": None,
				"body": None,
			},
			":setf": {
				"name": special_setf,
				"argumentNames": None,
				"body": None,
			},
			"function": {
				"name": special_function,
				"argumentNames": None,
				"body": None,
			},
			"if": {
				"name": special_if,
				"argumentNames": None,
				"body": None,
			},
			"let": {
				"name": special_let,
				"argumentNames": None,
				"body": None,
			},
			"progn": {
				"name": special_progn,
				"argumentNames": None,
				"body": None,
			},
			"quote": {
				"name": special_quote,
				"argumentNames": None,
				"body": None,
			},
		},
	}
	load(environment, "basic.lisp")
	return environment

def copyEnvironment(environment):
	copy =  {}
	for key in environment:
		copy[key] = environment[key]
	return copy

def load(environment, inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	reader = RootReader()
	while nextCharacter:
		result = reader.readNextCharacter(nextCharacter)
		if result:
			reader = RootReader()
			evaluate(environment, result)
		else:
			nextCharacter = input.read(1)

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
	if expression == NIL:
		return NIL
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

class Evaluator:
	def __init__(self):
		pass
