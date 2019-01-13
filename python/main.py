import sys
import LispFileReader
import Evaluator
import Printer
from repl import repl

def parseCommandLineFlags(argv):
	if argv[1] == "-q":
		return "quiet", argv[2]
	else:
		return "normal", argv[1]

mode, filename = parseCommandLineFlags(sys.argv)

reader = LispFileReader.LispFileReader(filename)
evaluator = Evaluator.Evaluator()
printer = Printer.Printer(mode)

repl(reader, evaluator, printer)

