from evaluate import evaluate, evaluate, createStandardEnvironment
from Node import Symbol, isSymbol, NIL
from read import read
from reader import expressionToString
from Stream import Stream

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
	assert(isSymbol(evaluate(environment, Symbol("a"))))

def test_addition():
	environment = createStandardEnvironment()
	expression = createExpressionFromString("(+ 2 3) ")
	assert(isSymbol(evaluate(environment, expression), "5"))

def test_defun_sum():
	environment = createStandardEnvironment()
	defineSum = createExpressionFromString("(defun sum (x y) (+ x y)) ")
	evaluate(environment, defineSum)
	callSum = createExpressionFromString("(setf result (sum 2 3)) ")
	evaluate(environment, callSum)
	assert(isSymbol(environment["result"], "5"))

def test_format_HelloWorld():
	assertStdout('(format t "hello, world~%") ', "hello, world\n")

def test_format_integer():
	assertStdout('(format t "2 + 3 = ~a" 5) ', "2 + 3 = 5")

def test_let():
	code = """
		(setf b 2)
		(format t "b = ~a~%" b)

		(let ((b 3))
			(format t "b = ~a~%" b)
		)

		(format t "b = ~a~%" b)
	"""
	assertStdout(code, "b = 2\nb = 3\nb = 2\n")

def test_if_true():
	code = """
		(if t
			(format t "true")
			(format t "false")
		)
	"""	
	assertStdout(code, "true")

def test_if_false():
	code = """
		(if nil
			(format t "true")
			(format t "false")
		)
	"""	
	assertStdout(code, "false")

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

