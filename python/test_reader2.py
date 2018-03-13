from test_utilities import *
from reader2 import *

def test_SymbolReader2_singleLetter():
	reader = SymbolReader2("a")
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "a")
test_SymbolReader2 = {
	"test_SymbolReader2_singleLetter": test_SymbolReader2_singleLetter,
}
runTests(test_SymbolReader2)

def test_StringReader2_singleLetter():
	reader = StringReader2("\"")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter("\"")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "\"a\"")
test_StringReader2 = {
	"test_StringReader2_singleLetter": test_StringReader2_singleLetter,
}
runTests(test_StringReader2)

def test_ConsReader2_emptyList():
	reader = ConsReader2("(")

	shouldBeNull = reader.readNextCharacter(")")
	assert(not shouldBeNull)

	result = reader.readNextCharacter(" ")
	assert(result == None)
def test_ConsReader2_singleElementList():
	reader = ConsReader2("(")
	assert(reader.stage == "waitingForElement")

	shouldBeNull = reader.readNextCharacter("a")
	assert(reader.stage == "readingElement")
	assert(not shouldBeNull)

	shouldBeNull = reader.readNextCharacter(")")
	assert(not shouldBeNull)

	cons = reader.readNextCharacter(" ")
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
				return reader.getValue()
	assert(False)
def parseString2(string):
	reader = newReader2(string[0])

	for character in string[1:]:
		lastResult = reader.readNextCharacter(character)
		if lastResult:
			assert(character == string[-1])
			return lastResult
	assert(False)
def test_ConsReader2_twoElementList():
	cons = parseString2("(a b) ")

	assert(cons.getType() == "cons")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr().getType() == "cons")
	assert(cons.getCdr().getCar().getType() == "symbol")
	assert(cons.getCdr().getCar().getValue() == "b")
	assert(cons.getCdr().getCdr() == None)
test_ConsReader = {
	"test_ConsReader2_emptyList": test_ConsReader2_emptyList,
	"test_ConsReader2_singleElementList": test_ConsReader2_singleElementList,
	"test_ConsReader2_twoElementList": test_ConsReader2_twoElementList,
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
	"test_readExpressions_quoteSymbol": test_readExpressions_quoteSymbol,
	"test_readExpressions_quasiquoteSymbol": test_readExpressions_quasiquoteSymbol,
	"test_readExpressions_quasiquoteAndComma": test_readExpressions_quasiquoteAndComma,
}
runTests(test_readExpressions)

