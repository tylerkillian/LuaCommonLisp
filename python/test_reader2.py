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

def parseString2(string):
	reader = newReader2(string[0])

	for character in string[1:]:
		lastResult = reader.readNextCharacter(character)
		if lastResult:
			assert(character == string[-1])
			return lastResult
	assert(False)
def test_ConsReader2_emptyList():
	result = parseString2("() ")
	assert(result == NIL)
def test_ConsReader2_singleElementList():
	cons = parseString2("(a) ")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr() == NIL)
def test_ConsReader2_twoElementList():
	cons = parseString2("(a b) ")
	assert(cons.getType() == "cons")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr().getType() == "cons")
	assert(cons.getCdr().getCar().getType() == "symbol")
	assert(cons.getCdr().getCar().getValue() == "b")
	assert(cons.getCdr().getCdr() == NIL)
test_ConsReader = {
	"test_ConsReader2_emptyList": test_ConsReader2_emptyList,
	"test_ConsReader2_singleElementList": test_ConsReader2_singleElementList,
	"test_ConsReader2_twoElementList": test_ConsReader2_twoElementList,
}
runTests(test_ConsReader)


def test_readExpressions_setf():
	tree = parseString2("(setf a 3) ")
	assert(treeToString2(tree) == "(setf a 3)")
def test_readExpressions_format():
	tree = parseString2("(format t \"a = ~a~%\" a) ")
	assert(treeToString2(tree) == "(format t \"a = ~a~%\" a)")
def test_readExpressions_quoteSymbol():
	tree = parseString2("'a ")
	assert(treeToString2(tree) == "(quote a)")
def test_readExpressions_quasiquoteSymbol():
	tree = parseString2("`a ")
	assert(treeToString2(tree) == "(quasiquote a)")
def test_readExpressions_quasiquoteAndComma():
	tree = parseString2("``(w ,x ,,y) ")
	assert(treeToString2(tree) == "``(w ,x ,,y)")
test_readExpressions = {
	"test_readExpressions_setf": test_readExpressions_setf,
	"test_readExpressions_format": test_readExpressions_format,
	"test_readExpressions_quoteSymbol": test_readExpressions_quoteSymbol,
	"test_readExpressions_quasiquoteSymbol": test_readExpressions_quasiquoteSymbol,
	#"test_readExpressions_quasiquoteAndComma": test_readExpressions_quasiquoteAndComma,
}
runTests(test_readExpressions)

