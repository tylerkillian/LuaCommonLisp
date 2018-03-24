from evaluate import evaluate
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

def test_formatHelloWorld():
	assertStdout('(format t "hello, world~%") ', "hello, world\n")

def test_formatInteger():
	assertStdout('(format t "2 + 3 = ~a" 5) ', "2 + 3 = 5")
