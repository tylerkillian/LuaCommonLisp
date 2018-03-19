from test_utilities import *
from reader import *

def test_SymbolReader_singleLetter():
	reader = SymbolReader("a")
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "a")
test_SymbolReader = {
	"test_SymbolReader_singleLetter": test_SymbolReader_singleLetter,
}
runTests(test_SymbolReader)

def test_StringReader_singleLetter():
	reader = StringReader("\"")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter("\"")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "\"a\"")
test_StringReader = {
	"test_StringReader_singleLetter": test_StringReader_singleLetter,
}
runTests(test_StringReader)

def parseString(string):
	reader = newReader(string[0])

	for character in string[1:]:
		lastResult = reader.readNextCharacter(character)
		if lastResult:
			assert(character == string[-1])
			return lastResult
	assert(False)
def test_ConsReader_emptyList():
	result = parseString("() ")
	assert(result == NIL)
def test_ConsReader_singleElementList():
	cons = parseString("(a) ")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr() == NIL)
def test_ConsReader_twoElementList():
	cons = parseString("(a b) ")
	assert(cons.getType() == "cons")
	assert(cons.getCar().getType() == "symbol")
	assert(cons.getCar().getValue() == "a")
	assert(cons.getCdr().getType() == "cons")
	assert(cons.getCdr().getCar().getType() == "symbol")
	assert(cons.getCdr().getCar().getValue() == "b")
	assert(cons.getCdr().getCdr() == NIL)
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
def test_readExpressions_expandBackquote_symbol():
	tree = parseString("`a ")
	treeAfterMacroExpansion = expandBackquoteMacro(tree)
	assert(treeToString(treeAfterMacroExpansion) == "'a")
def test_readExpressions_expandBackquote_nestedBackquotesCommas():
	tree = parseString("``(w ,x ,,y) ")
	treeAfterMacroExpansion = expandBackquoteMacro(tree)
	assert(treeToString(treeAfterMacroExpansion) == "(list 'list ''w 'x y)")
test_readExpressions = {
	"test_readExpressions_setf": test_readExpressions_setf,
	"test_readExpressions_format": test_readExpressions_format,
	"test_readExpressions_quoteSymbol": test_readExpressions_quoteSymbol,
	"test_readExpressions_quasiquoteSymbol": test_readExpressions_quasiquoteSymbol,
	"test_readExpressions_quasiquoteAndComma": test_readExpressions_quasiquoteAndComma,
	"test_readExpressions_expandBackquote_symbol": test_readExpressions_expandBackquote_symbol,
	"test_readExpressions_expandBackquote_nestedBackquotesCommas": test_readExpressions_expandBackquote_nestedBackquotesCommas,
}
runTests(test_readExpressions)


def test_backquoteExpansion_getBackquoteDepth0():
	tree = parseString("a ")
	assert(getBackquoteDepth(tree) == 0)
def test_backquoteExpansion_getBackquoteDepth1():
	tree = parseString("`a ")
	assert(getBackquoteDepth(tree) == 1)
def test_backquoteExpansion_getBackquoteDepth2():
	tree = parseString("`(a `(b ,,c) ,d) ")
	assert(getBackquoteDepth(tree) == 2)
def test_backquoteExpansion_getInnerBackquote():
	tree = parseString("`(a `(b ,,c) ,d) ")
	innerBackquote = tree.getCdr().getCar().getCdr().getCar()
	assert(getInnerBackquote(tree) == innerBackquote)
test_backquoteExpansion = {
	"test_backquoteExpansion_getBackquoteDepth0": test_backquoteExpansion_getBackquoteDepth0,
	"test_backquoteExpansion_getBackquoteDepth1": test_backquoteExpansion_getBackquoteDepth1,
	"test_backquoteExpansion_getBackquoteDepth2": test_backquoteExpansion_getBackquoteDepth2,
	"test_backquoteExpansion_getInnerBackquote": test_backquoteExpansion_getInnerBackquote,
}
runTests(test_backquoteExpansion)

