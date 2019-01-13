import sys
from reader import *
from evaluate import evaluate, createStandardEnvironment
from repl import repl

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
		nextCharacter = self.input.read(1)
		while nextCharacter:
			expression = self.reader.readNextCharacter(nextCharacter)
			if expression:
				self.reader = RootReader()
				return expression
			else:
				nextCharacter = self.input.read(1)

class Evaluator:
	def __init__(self):
		self.environment = createStandardEnvironment()
	def evaluate(self, expression):
		return evaluate(self.environment, expression)

class Printer:
	def __init__(self, mode):
		self.mode = mode
	def print(self, value):
		if self.mode == "normal":
			print(expressionToString(result))
		
mode, filename = parseCommandLineFlags(sys.argv)

reader = LispFileReader(filename)
evaluator = Evaluator()
printer = Printer(mode)

repl(reader, evaluator, printer)

