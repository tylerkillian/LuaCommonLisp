import getCommandLineFlags
import LispFileReader
import Evaluator
import Printer
from repl import repl

flags = getCommandLineFlags.getCommandLineFlags()

reader = LispFileReader.LispFileReader(flags["filename"])
evaluator = Evaluator.Evaluator()
printer = Printer.Printer(flags["mode"])

repl(reader, evaluator, printer)

