import sys
from reader import *
from evaluate import evaluate, createStandardEnvironment

def isTestCommand(expression):
	if expressionToString(expression) == "(__run-all-tests__)":
		return True
	else:
		return False

def launchAllTests(environment):
	for functionName in environment["functions"]:
		if functionName[0:5] == "test-":
			print(functionName)
			testExpression = Cons(Symbol(functionName), NIL)
			evaluate(environment, testExpression)

def lisp(mode, inputFile):
	input = open(inputFile, "r")
	nextCharacter = input.read(1)
	environment = createStandardEnvironment()
	reader = RootReader()
	while nextCharacter:
		expression = reader.readNextCharacter(nextCharacter)
		if expression:
			reader = RootReader()

			if isTestCommand(expression):
				launchAllTests(environment)
			else:
				result = evaluate(environment, expression)
				if mode == "normal":
					print(expressionToString(result))
		else:
			nextCharacter = input.read(1)

def parseCommandLineFlags(argv):
	if argv[1] == "-q":
		return "quiet", argv[2]
	else:
		return "normal", argv[1]

class LispFileReader:
	def __init__(self, inputFile):
		self.input = open(inputFile, "r")
		self.reader = RootReader()
	def read(self):
		nextCharacter = input.read(1)
		while nextCharacter:
			expression = self.reader.readNextCharacter(nextCharacter)
			if expression:
				self.reader = RootReader()
				return expression
			else:
				nextCharacter = input.read(1)

class Evaluator:
	def __init__(self):
		self.environment = createStandardEnvironment()
	def evaluate(self, expression):
		if isTestCommand(expression):
			launchAllTests(self.environment)
		else
			return evaluate(self.environment, expression)

class Printer:
	def __init__(self, mode):
		self.mode = mode
	def print(self, value):
		if self.mode == "normal":
			print(expressionToString(result))
		

mode, filename = parseCommandLineFlags(sys.argv)
lisp(mode, filename)

#reader = LispFileReader(filename)
#evaluator = Evaluator()
#printer = Printer(mode)
#
#repl(reader, evaluator, printer)

