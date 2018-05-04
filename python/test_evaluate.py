from evaluate import *
from Node import Symbol, isSymbol, NIL
from read import read
from reader import expressionToString
from Stream import Stream

# BEGIN ATOMS

TOLERANCE = 1.0e-6

def test_addition():
	result = function_addition({}, {}, [Number(1), Number(2), Number(3), Number(4), Number(5)])
	assert(isNumber(result))
	assert(abs(getNumberValue(result) - 15) < TOLERANCE)

def test_addition_noArguments():
	result = function_addition({}, {}, [])
	assert(isNumber(result))
	assert(abs(getNumberValue(result)) < TOLERANCE)

def test_apply():
	functionToApply = FunctionPointer("+")
	environment = {
		"functions": {
			"+": {
				"name": function_addition,
				"argumentNames": None,
				"body": None,
			}
		}
	}
	arguments = [functionToApply, Number(1), Number(2), Expression(Number(3), Number(4), Number(5))]
	result = function_apply(environment, {}, arguments)
	assert(abs(getNumberValue(result) - 15) < TOLERANCE)

def test_apply_nilArgument():
	functionToApply = FunctionPointer("+")
	environment = {
		"nil": NIL,
		"functions": {
			"+": {
				"name": function_addition,
				"argumentNames": None,
				"body": None,
			}
		}
	}
	arguments = [functionToApply, NIL]
	result = function_apply(environment, {}, arguments)
	assert(abs(getNumberValue(result)) < TOLERANCE)

def test_defun_sum():
	environment = {
		"functions": {
			"+": {
				"name": function_addition,
				"argumentNames": None,
				"body": None,
			}
		}
	}
	arguments = [Symbol("sum"), Expression(Symbol("x"), Symbol("y")), Expression(Symbol("+"), Symbol("x"), Symbol("y"))]
	special_defun(environment, {}, arguments)
	callSum = Expression(Symbol("sum"), Number(2), Number(3))
	result = evaluate(environment, callSum)
	assert(abs(getNumberValue(result) - 5) < TOLERANCE)

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

def test_assoc_found():
	code = """
		(setf a '((d . 3) (e . 4) (f . 5)))
		(list (assoc 'f a) (assoc 'd a) (assoc 'e a))
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "((f . 5) (d . 3) (e . 4))")
	assert(stdout == "")

def test_assoc_notFound():
	code = """
		(setf a '((d . 3) (e . 4) (f . 5)))
		(assoc 'g a)
	"""
	returnValue, stdout = runCode(code)
	assert(expressionToString(returnValue) == "nil")
	assert(stdout == "")

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

def test_let_singleValue():
	code = """
		(setf b 2)
		(format t "b = ~a~%" b)

		(let ((b 3))
			(format t "b = ~a~%" b)
		)

		(format t "b = ~a~%" b)
	"""
	assertStdout(code, "b = 2\nb = 3\nb = 2\n")

def test_let_multipleValues():
	code = """
		(let ((a 2) (b 3))
			(format t "a = ~a~%" a)
			(format t "b = ~a~%" b)
		)
	"""
	assertStdout(code, "a = 2\nb = 3\n")

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

