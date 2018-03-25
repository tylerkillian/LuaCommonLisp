from evaluate import evaluate, evaluate2, createStandardEnvironment
from Node import Symbol, isSymbol
from read import read
from reader import expressionToString
from Stream import Stream

def assertStdout(inputString, result):
	inputStream = Stream(inputString)
	expression = read(inputStream)
	outputStream = Stream()
	environment = {
		"*standard-output*": outputStream,
	}
	evaluate(environment, expression)
	assert(environment["*standard-output*"].read() == result)

def createExpressionFromString(string):
	inputStream = Stream(string)
	return read(inputStream)

def test_formatHelloWorld():
	assertStdout('(format t "hello, world~%") ', "hello, world\n")

def test_formatInteger():
	assertStdout('(format t "2 + 3 = ~a" 5) ', "2 + 3 = 5")

def test_evaluateSymbol():
	environment = createStandardEnvironment()
	environment["a"] = Symbol("1")
	assert(isSymbol(evaluate2(environment, Symbol("a"))))

def test_addition():
	environment = createStandardEnvironment()
	expression = createExpressionFromString("(+ 2 3) ")
	assert(isSymbol(evaluate2(environment, expression), "5"))

def test_defunSum():
	environment = createStandardEnvironment()
	defineSum = createExpressionFromString("(defun sum (x y) (+ x y)) ")
	evaluate2(environment, defineSum)
	callSum = createExpressionFromString("(setf result (sum 2 3)) ")
	evaluate2(environment, callSum)
	assert(isSymbol(environment["result"], "5"))

def assertStdout2(inputString, result):
	inputStream = Stream(inputString)
	expression = read(inputStream)
	outputStream = Stream()
	environment = createStandardEnvironment()
	environment["*standard-output*"] = outputStream
	evaluate2(environment, expression)
	assert(environment["*standard-output*"].read() == result)

def test_formatHelloWorld2():
	assertStdout2('(format t "hello, world~%") ', "hello, world\n")

def test_formatInteger2():
	assertStdout2('(format t "2 + 3 = ~a" 5) ', "2 + 3 = 5")
