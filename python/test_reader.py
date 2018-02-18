from test_utilities import *
from reader import *

def test_ConsReader_emptyList():
	root = Node("root")
	readerStack = []

	ConsReader(readerStack, "(", root)
	assert(len(readerStack) == 1)

	reader = readerStack[-1]
	root.addChild(reader.getValue())

	shouldBeNull = readerStack[-1].readNextCharacter(readerStack, ")")
	assert(not shouldBeNull)
	assert(readerStack[-1] == reader)
	assert(not reader.isDone())

	shouldBeSpace = readerStack[-1].readNextCharacter(readerStack, " ")
	assert(shouldBeSpace == " ")
	assert(reader.isDone())

	assert(len(readerStack) == 0)

	assert(root.getNumChildren() == 1)
	assert(root.getChild(0).getNumChildren() == 2)
	assert(root.getChild(0).getChild(0).getName() == "symbol_nil")
	assert(root.getChild(0).getChild(1).getName() == "symbol_nil")
def test_ConsReader_singleElementList():
	root = Node("root")
	readerStack = []

	ConsReader(readerStack, "(", root)
	assert(len(readerStack) == 1)

	consReader = readerStack[-1]
	root.addChild(consReader.getValue())

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

	assert(root.getNumChildren() == 1)
	cons = root.getChild(0)
	assert(cons.getNumChildren() == 2)
	assert(cons.getChild(0).getName() == "symbol_a")
	assert(cons.getChild(1).getName() == "symbol_nil")
def parseString(string):
	root = Node("root")
	readerStack = []
	newReader(readerStack, string[0], root)
	assert(len(readerStack) == 1)
	root.addChild(readerStack[-1].getValue())

	for character in string[1:]:
		characterUsed = False
		while not characterUsed:
			lastResult = readerStack[-1].readNextCharacter(readerStack, character)
			if lastResult != character:
				characterUsed = True
			if len(readerStack) == 0:
				assert(character == string[-1])
				assert(character == lastResult)
				return root
	assert(False)
def test_ConsReader_twoElementList():
	root = parseString("(a b) ")

	assert(root.getNumChildren() == 1)
	assert(root.getChild(0).getName() == "cons")
	assert(root.getChild(0).getNumChildren() == 2)
	assert(root.getChild(0).getChild(0).getName() == "symbol_a")
	assert(root.getChild(0).getChild(1).getName() == "cons")
	assert(root.getChild(0).getChild(1).getNumChildren() == 2)
	assert(root.getChild(0).getChild(1).getChild(0).getName() == "symbol_b")
	assert(root.getChild(0).getChild(1).getChild(1).getName() == "symbol_nil")
test_ConsReader = {
	"test_ConsReader_emptyList": test_ConsReader_emptyList,
	"test_ConsReader_singleElementList": test_ConsReader_singleElementList,
	"test_ConsReader_twoElementList": test_ConsReader_twoElementList,
}
runTests(test_ConsReader)


def test_readExpressions_setf():
	root = parseString("(setf a 3) ")
	assert(treeToString(root) == "(setf a 3)")
def test_readExpressions_format():
	root = parseString("(format t \"a = ~a~%\" a) ")
	assert(treeToString(root) == "(format t \"a = ~a~%\" a)")
test_readExpressions = {
	"test_readExpressions_setf": test_readExpressions_setf,
	"test_readExpressions_format": test_readExpressions_format,
}
runTests(test_readExpressions)

