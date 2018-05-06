from evaluate import *
from Node import Symbol, isSymbol, NIL
from read import read
from reader import expressionToString
from Stream import Stream

# BEGIN ATOMS

TOLERANCE = 1.0e-6

class FakeAddition:
	def __init__(self):
		pass
	def __call__(self, environment, metadata, arguments):
		numArguments = len(arguments)
		if numArguments == 0:
			return Number(0)
		elif numArguments == 5:
			assert(isNumber(arguments[0], 1, TOLERANCE))
			assert(isNumber(arguments[1], 2, TOLERANCE))
			assert(isNumber(arguments[2], 3, TOLERANCE))
			assert(isNumber(arguments[3], 4, TOLERANCE))
			assert(isNumber(arguments[4], 5, TOLERANCE))
			return Number(15)

class FakeEnvironment:
	def __init__(self):
		self.lookup = {
			"functions": {
				"+": {
					"name": FakeAddition(),
					"argumentNames": None,
					"body": None,
				}
			}
		}
	def __contains__(self, key):
		return key in self.lookup
	def __getitem__(self, key):
		assert(self.lookup[key])
		return self.lookup[key]
	def __setitem__(self, key, value):
		self.lookup[key] = value
	def copy(self):
		theCopy = FakeEnvironment()
		for key in self.lookup:
			theCopy[key] = self.lookup[key]
		return theCopy
	def getFunctionPointer(self, name):
		assert(self.lookup["functions"][name])
		return FunctionPointer(name)

class FakeEvaluator:
	def __init__(self, responses):
		self.history = []
		self.responses = responses
	def __call__(self, environment, expression):
		self.history.append(expression)
		assert(self.responses[expression])
		return self.responses[expression]
	def getHistory(self):
		return self.history

class FakeSum:
	def __init__(self):
		pass
	def __call__(self, environment, metadata, arguments):
		assert(len(arguments) == 2)
		assert(isNumber(arguments[0], 2, TOLERANCE))
		assert(isNumber(arguments[1], 3, TOLERANCE))
		return Number(5)

class FakeSumMaker:
	def __init__(self):
		pass
	def __call__(self, argumentNames, body):
		assert(len(argumentNames) == 2)
		assert(argumentNames[0] == "x")
		assert(argumentNames[1] == "y")
		assert(len(body) == 1)
		assert(expressionToString(body[0]) == "(+ x y)")
		return FakeSum()

def fakeIsTrue(expression):
	if expression == "true":
		return True
	else:
		return False

def test_addition():
	result = function_addition({}, {}, [Number(1), Number(2), Number(3), Number(4), Number(5)])
	assert(isNumber(result))
	assert(abs(getNumberValue(result) - 15) < TOLERANCE)

def test_addition_noArguments():
	result = function_addition({}, {}, [])
	assert(isNumber(result))
	assert(abs(getNumberValue(result)) < TOLERANCE)

def test_apply():
	environment = FakeEnvironment()
	functionToApply = environment.getFunctionPointer("+")
	arguments = [functionToApply, Number(1), Number(2), Expression(Number(3), Number(4), Number(5))]
	result = function_apply(environment, {}, arguments)
	assert(abs(getNumberValue(result) - 15) < TOLERANCE)

def test_apply_nilArgument():
	environment = FakeEnvironment()
	functionToApply = environment.getFunctionPointer("+")
	arguments = [functionToApply, NIL]
	result = function_apply(environment, {}, arguments)
	assert(abs(getNumberValue(result)) < TOLERANCE)

def test_defun_sum():
	environment = FakeEnvironment()
	special_defun = Defun(FakeSumMaker())

	arguments = [Symbol("sum"), Expression(Symbol("x"), Symbol("y")), Expression(Symbol("+"), Symbol("x"), Symbol("y"))]
	special_defun(environment, {}, arguments)
	result = environment["functions"]["sum"]["name"](environment, {}, [Number(2), Number(3)])
	assert(abs(getNumberValue(result) - 5) < TOLERANCE)

def test_if_true():
	evaluate = FakeEvaluator({
		"this evaluates to true" : "true",
		"first expression" : "first expression evaluated",
		"second expression" : "second expression evaluated",
	})
	environment = FakeEnvironment()
	special_if = If(fakeIsTrue)

	arguments = ["this evaluates to true", "first expression", "second expression"]
	result = special_if(evaluate, environment, arguments)
	assert(result == "first expression evaluated")
	history = evaluate.getHistory()
	assert(len(history) == 2)
	assert(history[0] == "this evaluates to true")
	assert(history[1] == "first expression")

def test_if_false():
	evaluate = FakeEvaluator({
		"this evaluates to false" : "false",
		"first expression" : "first expression evaluated",
		"second expression" : "second expression evaluated",
	})
	environment = FakeEnvironment()
	special_if = If(fakeIsTrue)

	arguments = ["this evaluates to false", "first expression", "second expression"]
	result = special_if(evaluate, environment, arguments)
	assert(result == "second expression evaluated")
	history = evaluate.getHistory()
	assert(len(history) == 2)
	assert(history[0] == "this evaluates to false")
	assert(history[1] == "second expression")

def test_let_singleValue():
	def evaluate(environment, expression):
		if expression == "get value of b":
			return environment["b"]
		elif isNumber(expression, 3):
			return Number("3")
		else:
			assert(False)
	environment = FakeEnvironment()
	environment["b"] = Number(2)
	special_let = Let()

	arguments = [Expression(Expression(Symbol("b"), Number("3"))), "get value of b"]
	result = special_let(evaluate, environment, arguments)
	assert(isNumber(result, 3))
	assert(isNumber(environment["b"], 2))

def test_let_multipleValues():
	def evaluate(environment, expression):
		if expression == "check value of a":
			assert(isNumber(environment["a"], 2))
			return environment["a"]
		elif expression == "check value of b":
			assert(isNumber(environment["b"], 3))
			return environment["b"]
		elif isNumber(expression, 2):
			return Number("2")
		elif isNumber(expression, 3):
			return Number("3")
		else:
			assert(False)
	environment = FakeEnvironment()
	special_let = Let()

	arguments = [
		Expression(
			Expression(Symbol("a"), Number("2")),
			Expression(Symbol("b"), Number("3"))
		),
		"check value of a",
		"check value of b"
	]
	result = special_let(evaluate, environment, arguments)
	assert(isNumber(result, 3))
	assert(not "a" in environment)
	assert(not "b" in environment)

# END ATOMS

def createExpressionFromString(string):
	inputStream = Stream(string)
	return read(inputStream)

def runCode(code):
	inputStream = Stream(code)
	environment = createStandardEnvironment()
	outputStream = Stream()
	environment["*standard-output*"] = outputStream
	nextExpression = read(inputStream)
	while nextExpression:
		lastReturnValue = evaluate(environment, nextExpression)
		nextExpression = read(inputStream)
	stdout = environment["*standard-output*"].read()
	return lastReturnValue, stdout

def assertStdout(inputString, result):
	inputStream = Stream(inputString)
	environment = createStandardEnvironment()
	outputStream = Stream()
	environment["*standard-output*"] = outputStream
	nextExpression = read(inputStream)
	while nextExpression:
		evaluate(environment, nextExpression)
		nextExpression = read(inputStream)
	assert(environment["*standard-output*"].read() == result)

def test_evaluate_symbol():
	environment = createStandardEnvironment()
	environment["a"] = Symbol("1")
	assert(isSymbol(evaluate(environment, Symbol("a")), "1"))

def test_format_HelloWorld():
	assertStdout('(format t "hello, world~%") ', "hello, world\n")

def test_format_integer():
	assertStdout('(format t "2 + 3 = ~a" 5) ', "2 + 3 = 5")

def test_function():
	code = """
		(function rest)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "rest")
	assert(stdout == "")

def test_function_readMacro():
	code = """
		#'rest
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "rest")
	assert(stdout == "")

def test_progn():
	code = """
		(progn
			(format t "1~%")
			(format t "2~%")
			(format t "3~%")
			(format t "4~%")
			(format t "5~%")
		)
	"""
	returnValue, stdout = runCode(code)
	assert(returnValue == NIL)
	assert(stdout == "1\n2\n3\n4\n5\n")

def test_list():
	code = """
		(list 1 2 3 4 5)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(1 2 3 4 5)")
	assert(stdout == "")

def test_append():
	code = """
		(append (list 1 2 3) (list 4 5))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(1 2 3 4 5)")
	assert(stdout == "")

def test_quote():
	code = """
		'a
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "a")
	assert(stdout == "")

def test_defmacro():
	code = """
		(defmacro when (condition &rest body)
			`(if ,condition
				(progn ,@body)
				nil
			)	
		)

		(when t
			(format t "one~%")
			(format t "two~%")
			(format t "three~%")
		)
	"""
	returnValue, stdout = runCode(code)
	assert(returnValue == NIL)
	assert(stdout == "one\ntwo\nthree\n")

def test_cons():
	code = """
		(cons 1 '(2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(1 2 3)")
	assert(stdout == "")

def test_consp_true():
	code = """
		(consp '(1 2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_consp_false():
	code = """
		(consp 'a)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_car_simpleList():
	code = """
		(car '(1 2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "1")
	assert(stdout == "")

def test_car_emptyList():
	code = """
		(car '())
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_cdr_simpleList():
	code = """
		(cdr '(1 2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(2 3)")
	assert(stdout == "")

def test_cdr_singleElement():
	code = """
		(cdr '(1))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_cdr_emptyList():
	code = """
		(cdr '())
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_eql_sameSymbol():
	code = """
		(eql 'abc 'abc)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_eql_sameObject():
	code = """
		(setf a '(1 2 3))
		(setf b a)
		(eql a b)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_eql_differentObject():
	code = """
		(setf a '(1 2 3))
		(setf b '(1 2 3))
		(eql a b)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_eql_differentTypes():
	code = """
		(setf a '(1 2 3))
		(setf b 1)
		(eql a b)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_eql_differentSymbols():
	code = """
		(eql 'a 'b)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_equal_true():
	code = """
		(equal '(1 '(2 3) 4) '(1 '(2 3) 4))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_equal_false():
	code = """
		(equal '(1 '(2 3) 4) '(1 '(2 5) 4))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_greaterThan_true():
	code = """
		(> 2 1)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_greaterThan_false():
	code = """
		(> 1 2)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_lambda_addThree():
	code = """
		(setf addThree (lambda (x) (+ x 3)))
		(apply addThree '(1))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "4")
	assert(stdout == "")
		
def test_lambda_addTwoReadMacro():
	code = """
		(apply #'(lambda (x) (+ x 2)) '(4))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "6")
	assert(stdout == "")

def test_lambda_rest():
	code = """
		(apply #'(lambda (x &rest yz) (+ x (car yz) (cadr yz))) '(4 5 6))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "15")
	assert(stdout == "")

def test_null_true():
	code = """
		(null nil)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_null_false():
	code = """
		(null 'a)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_rest_simpleList():
	code = """
		(rest '(1 2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(2 3)")
	assert(stdout == "")

def test_rest_singleElement():
	code = """
		(rest '(1))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_rest_emptyList():
	code = """
		(rest '())
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_reverse_simpleList():
	code = """
		(reverse '(1 2 3 4 5))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(5 4 3 2 1)")
	assert(stdout == "")

def test_subtraction():
	environment = createStandardEnvironment()
	expression = createExpressionFromString("(- 1 2 3 4) ")
	assert(isSymbol(evaluate(environment, expression), "-8"))

def test_zerop_true():
	code = """
		(zerop 0)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_zerop_false():
	code = """
		(zerop 1)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_hasCars_true():
	code = """
		(has-cars '((a b c) (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_hasCars_false():
	code = """
		(has-cars '((a b c) () (d e f)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_getAllCars_thereAreCars():
	code = """
		(get-all-cars '((a b c) (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(a 1)")
	assert(stdout == "")

def test_getAllCars_notAllListsHaveCars():
	code = """
		(get-all-cars '((b c) () (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_getAllCdrs_thereAreCdrs():
	code = """
		(get-all-cdrs '((a b c) (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "((b c) (2 3))")
	assert(stdout == "")

def test_getAllCdrs_notAllListsHaveCdrs():
	code = """
		(get-all-cdrs '((b c) (d) (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_hasCdrs_true():
	code = """
		(has-cdrs '((a b c) (1 2 3)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "t")
	assert(stdout == "")

def test_hasCdrs_false():
	code = """
		(has-cdrs '((a b c) (d) (d e f)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_mapcar_emptyList():
	code = """
		(mapcar #'(lambda (x) (+ x 10)) nil)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_mapcarViaApply_emptyList():
	code = """
		(apply #'mapcar #'(lambda (x) (+ x 10)) '(()))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

def test_mapcarViaApply_singleNumber():
	code = """
		(apply #'mapcar #'(lambda (x) (+ x 10)) '((1)))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(11)")
	assert(stdout == "")

def test_mapcar_addTen():
	code = """
		(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "(11 12 13)")
	assert(stdout == "")

