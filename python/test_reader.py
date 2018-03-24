from reader import *

def test_SymbolReader_singleLetter():
	reader = SymbolReader("a")
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "a")

def test_StringReader_singleLetter():
	reader = StringReader("\"")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter("\"")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result.getValue() == "\"a\"")

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
def test_readExpressions_quasiquoteAndCommaAt():
	tree = parseString("``(w ,x ,@y) ")
	assert(treeToString(tree) == "``(w ,x ,@y)")


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
def test_backquoteExpansion_replaceInnerBackquote():
	originalTree = parseString("`(a `(b ,,c) ,d) ")
	replacement = parseString("e ")
	newTree = replaceInnerBackquote(originalTree, replacement)
	assert(treeToString(newTree) == "`(a e ,d)")
def test_backquoteExpansion_expandBackquoteMacro_symbol():
	tree = parseString("`a ")
	treeWithExpandedBackquote = expandBackquoteMacro(tree)
	assert(treeToString(treeWithExpandedBackquote) == "'a")
def test_backquoteExpansion_expandBackquoteMacro_expression():
	tree = parseString("`(a ,@b ,c) ")
	treeWithExpandedBackquote = expandBackquoteMacro(tree)
	assert(treeToString(treeWithExpandedBackquote) == "(append (list 'a) b (list c))")
def test_backquoteExpansion_expandBackquoteMacro_nestedExpression():
	tree = parseString("`(a `(b ,,c) ,d) ")
	treeWithExpandedBackquote = expandBackquoteMacro(tree)
	assert(treeToString(treeWithExpandedBackquote) == "(append (list 'a) (list (append (list 'append) (list (append (list 'list) (list (append (list 'quote) (list 'b))))) (list (append (list 'list) (list c))))) (list d))")
