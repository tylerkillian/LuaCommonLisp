from evaluate import evaluate, createStandardEnvironment

class Form:
	def __init

class Evaluator:
	def __init__(self):
		self.environment = createStandardEnvironment()
	def evaluate(self, expression):
		return evaluate(self.environment, expression)
	def new_evaluate(self, form):
		if form.isSymbol():
			return self.evaluateSymbol(form)
		elif form.isConses():
			return self.evaluateConses(form)
		else:
			assert(form.isSelfEvaluatingObject())
			return form


def refevaluate(environment, expression):
	if isTestCommand(expression):
		launchAllTests(environment)
		return

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


#class Evaluator:
#    def __init__(self, environment):
#        self.environment = environment
#    def __call__(self, form):
#        return evaluate(self.environment, form)

