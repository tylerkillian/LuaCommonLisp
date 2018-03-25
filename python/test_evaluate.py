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
	expression = createExpressionFromString("(defun sum (x y) (+ x y)) ")
	evaluate2(environment, expression)



