from test_utilities import *
from reader import *

def test_ConsReader_emptyList():
	readerStack = []

	ConsReader(readerStack, "(")
	assert(len(readerStack) == 1)

	reader = readerStack[-1]

	shouldBeNull = readerStack[-1].readNextCharacter(readerStack, ")")
	assert(not shouldBeNull)
	assert(readerStack[-1] == reader)
	assert(not reader.isDone())

	shouldBeSpace = readerStack[-1].readNextCharacter(readerStack, " ")
	assert(shouldBeSpace == " ")
	assert(reader.isDone())

	assert(len(readerStack) == 0)

	cons = reader.getValue2()
	assert(cons.getCar() == None)
	assert(cons.getCdr() == None)
def test_ConsReader_singleElementList():
	readerStack = []

	ConsReader(readerStack, "(")
	assert(len(readerStack) == 1)

	consReader = readerStack[-1]

	shouldBeNull = readerStack[-1].readNextCharacter(readerStack, "a")
	assert(not shouldBeNull)
	assert(len(readerStack) == 2)
	assert(readerStack[0] == consReader)
	assert(not consReader.isDone())
	symbolReader = readerStack[-1]

	shouldBeParenthesis = readerStack[-1].readNextCharacter(readerStack, ")")
	assert(shouldBeParenthesis == ")")
	assert(len(readerStack) == 1)
	assert(readerStack[0] == consReader)
	assert(not consReader.isDone())
	assert(symbolReader.isDone())

	shouldBeNull = readerStack[-1].readNextCharacter(readerStack, ")")
	assert(not shouldBeNull)
	assert(len(readerStack) == 1)
	assert(readerStack[0] == consReader)
	assert(not consReader.isDone())

	shouldBeSpace = readerStack[-1].readNextCharacter(readerStack, " ")
	assert(shouldBeSpace == " ")
	assert(consReader.isDone())

	assert(len(readerStack) == 0)

	cons = consReader.getValue2()
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr() == None)
def parseString(string):
	readerStack = []
	reader = newReader(readerStack, string[0])
	assert(len(readerStack) == 1)

	for character in string[1:]:
		characterUsed = False
		while not characterUsed:
			lastResult = readerStack[-1].readNextCharacter(readerStack, character)
			if lastResult != character:
				characterUsed = True
			if len(readerStack) == 0:
				assert(character == string[-1])
				assert(character == lastResult)
				return reader.getValue2()
	assert(False)
def test_ConsReader_twoElementList():
	cons = parseString("(a b) ")

	assert(cons.getType() == "cons")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr().getType() == "cons")
	assert(cons.getCdr().getCar().getType() == "symbol")
	assert(cons.getCdr().getCar().getValue() == "b")
	assert(cons.getCdr().getCdr() == None)
test_ConsReader = {
	"test_ConsReader_emptyList": test_ConsReader_emptyList,
	"test_ConsReader_singleElementList": test_ConsReader_singleElementList,
	"test_ConsReader_twoElementList": test_ConsReader_twoElementList,
}
runTests(test_ConsReader)


def test_readExpressions_setf():
	tree = parseString("(setf a 3) ")
	assert(treeToString(tree) == "(setf a 3)")
def test_readExpressions_format():
	tree = parseString("(format t \"a = ~a~%\" a) ")
	assert(treeToString(tree) == "(format t \"a = ~a~%\" a)")
def test_readExpressions_quoteSymbol():
	tree = parseString("'a ")
	assert(treeToString(tree) == "'a")
def test_readExpressions_quasiquoteSymbol():
	tree = parseString("`a ")
	assert(treeToString(tree) == "`a")
def test_readExpressions_quasiquoteAndComma():
	tree = parseString("``(w ,x ,,y) ")
	assert(treeToString(tree) == "``(w ,x ,,y)")
test_readExpressions = {
	"test_readExpressions_setf": test_readExpressions_setf,
	"test_readExpressions_format": test_readExpressions_format,
	#"test_readExpressions_quoteSymbol": test_readExpressions_quoteSymbol,
	#"test_readExpressions_quasiquoteSymbol": test_readExpressions_quasiquoteSymbol,
	#"test_readExpressions_quasiquoteAndComma": test_readExpressions_quasiquoteAndComma,
}
runTests(test_readExpressions)

