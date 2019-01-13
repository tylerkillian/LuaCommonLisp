import sys
import LispFileReader
import Evaluator
from repl import repl

from evaluate import evaluate, createStandardEnvironment

def parseCommandLineFlags(argv):
	if argv[1] == "-q":
		return "quiet", argv[2]
	else:
		return "normal", argv[1]

class Printer:
	def __init__(self, mode):
		self.mode = mode
	def print(self, value):
		if self.mode == "normal":
			print(expressionToString(result))
		
mode, filename = parseCommandLineFlags(sys.argv)

reader = LispFileReader.LispFileReader(filename)
evaluator = Evaluator.Evaluator()
printer = Printer(mode)

repl(reader, evaluator, printer)

