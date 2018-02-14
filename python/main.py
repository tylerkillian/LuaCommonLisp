class Node():
	def __init__(self, name, parent = None):
		self.name = name
		self.parent = parent
		self.children = []
	def getName(self):
		return self.name
	def setName(self, name):
		self.name = name
	def getParent(self):
		return self.parent
	def getNumChildren(self):
		return len(self.children)
	def addChild(self, child):
		self.children.append(child)
	def getChild(self, childIdx):
		assert(childIdx < len(self.children))
		return self.children[childIdx]

def test_Node_constructNoParent():
	node = Node("root")
	assert(node.getName() == "root")
	assert(node.getParent() == None)
	assert(node.getNumChildren() == 0)
def test_Node_constructWithParent():
	node = Node("theChild", Node("theParent"))
	assert(node.getParent().getName() == "theParent")
def test_Node_getName():
	node = Node("root")
	assert(node.getName() == "root")
def test_Node_addChild():
	node = Node("root")
	node.addChild(Node("child0"))
	assert(node.getNumChildren() == 1)
	assert(node.getChild(0).getName() == "child0")
test_Node = {
	"test_Node_constructNoParent": test_Node_constructNoParent,
	"test_Node_constructWithParent": test_Node_constructWithParent,
	"test_Node_getName": test_Node_getName,
	"test_Node_addChild": test_Node_addChild,
}
def runTests(tests):
	for testName, testFunction in tests.items():
		print(testName)
		testFunction() 
runTests(test_Node)


def newReader(readerStack, initialCharacter, parentNode):
	if initialCharacter == "(":
		return ConsReader(readerStack, initialCharacter, parentNode)
	else:
		return SymbolReader(readerStack, initialCharacter, parentNode)

class RootReader():
	def __init__(self, readerStack):
		readerStack.append(self)
		self.value = Node("root")
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		if nextCharacter == "(":
			self.done = True
			readerStack.pop()
			newReader(readerStack, nextCharacter, self.value)
			return
		else:
			return nextCharacter


class SymbolReader():
	def __init__(self, readerStack, initialCharacter, parentNode = None):
		readerStack.append(self)
		self.value = Node("symbol_" + initialCharacter, parentNode)
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		if nextCharacter == " " or nextCharacter == ")":
			self.done = True
			readerStack.pop()
			return nextCharacter
		else:
			self.value.setName(self.value.getName() + nextCharacter)
			return

class StringReader():
	def __init__(self, readerStack, initialCharacter, parentNode = None):
		assert(initialCharacter == "\"")
		readerStack.append(self)
		self.value = Node("string_" + initialCharacter, parentNode)
		self.mode = "readingString"
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		if self.mode == "waitingForTerminalCharacter":
			self.done = True
			readerStack.pop()
			return nextCharacter
		elif nextCharacter == "\"":
			self.mode = "waitingForTerminalCharacter"
			return
		else:
			self.value.setName(self.value.getName() + nextCharacter)
			return

def isWhitespace(character):
	if character == " ":
		return True
	elif character == "\t":
		return True
	elif character == "\n":
		return True
	else:
		return False

class ConsReader():
	def __init__(self, readerStack, initialCharacter, parentNode = None):
		assert(initialCharacter == "(")
		readerStack.append(self)
		self.stage = "waitingForCar"
		self.value = Node("cons", parentNode)
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def read(self, string):
		for characterIdx in range(0, len(string)):
			character = string[characterIdx]
			result = self.readNextCharacter(character)
			if characterIdx < len(string) - 1:
				assert(result == "")
				assert(not self.isDone)
		return result
	def processStage_waitingForCar(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForCar")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			self.value.addChild(Node("symbol_nil", self.value))
			self.value.addChild(Node("symbol_nil", self.value))
			assert(self.value.getNumChildren() == 2)
			self.stage = "waitingForTerminalCharacter"
		else:
			self.stage = "waitingForDot"
			readCar = newReader(readerStack, nextCharacter, self.value)
			self.value.addChild(readCar.getValue())
			assert(self.value.getNumChildren() == 1)
	def beginReadingNextListElement(self, readerStack, characters):
		self.done = True
		readerStack.pop()
		readNextListElement = ConsReader(readerStack, "(", self.value)
		self.value.addChild(readNextListElement.getValue())
		assert(self.value.getNumChildren() == 2)
		for nextCharacter in characters:
			readNextListElement.readNextCharacter(readerStack, nextCharacter)
		return
	def processStage_waitingForDot(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForDot")
		if nextCharacter == ".":
			self.stage = "readingDot"
			return
		elif isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			self.value.addChild(Node("symbol_nil", self.value))
			assert(self.value.getNumChildren() == 2)
			self.stage = "waitingForTerminalCharacter"
			return
		else:
			return self.beginReadingNextListElement(readerStack, nextCharacter)
	def processStage_readingDot(self, readerStack, nextCharacter):
		assert(self.stage == "readingDot")
		if isWhitespace(nextCharacter):
			self.stage = "waitingForCdr"
			return
		else:
			return self.beginReadingNextListElement(readerStack, "." + nextCharacter)
	def processStage_waitingForCdr(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForCdr")
		if not isWhitespace(nextCharacter):
			assert(nextcharacter != ")")
			self.stage = "waitingForParenthesis"
			cdrReader = newReader(readerStack, nextCharacter, self.value)
	def processStage_waitingForParenthesis(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForParenthesis")
		if nextCharacter == ")":
			self.stage = "waitingForTerminalCharacter"
		else:
			assert(isWhitespace(nextCharacter))
	def processStage_waitingForTerminalCharacter(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForTerminalCharacter")
		self.done = True
		readerStack.pop()
		return nextCharacter
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		if self.stage == "waitingForCar":
			return self.processStage_waitingForCar(readerStack, nextCharacter)
		elif self.stage == "waitingForDot":
			return self.processStage_waitingForDot(readerStack, nextCharacter)
		elif self.stage == "readingDot":
			return self.processStage_readingDot(readerStack, nextCharacter)
		elif self.stage == "waitingForCdr":
			return self.processStage_waitingForCdr(readerStack, nextCharacter)
		elif self.stage == "waitingForParenthesis":
			return self.processStage_waitingForParenthesis(readerStack, nextCharacter)
		else:
			assert(self.stage == "waitingForTerminalCharacter")
			return self.processStage_waitingForTerminalCharacter(readerStack, nextCharacter)

def treeToString(node, addParenthesis = True):
	if node.getName() == "root":
		return treeToString(node.getChild(0))
	elif node.getName() == "cons":
		if node.getChild(1).getName() == "cons":
			result = treeToString(node.getChild(0)) + " " + treeToString(node.getChild(1), False)
		else:
			if node.getChild(1).getName() == "symbol_nil":
				result = treeToString(node.getChild(0))
			else:
				result = treeToString(node.getChild(0)) + " " + treeToString(node.getChild(1))
		if addParenthesis:
			result = "(" + result + ")" 
		return result
	elif node.getName()[0:7] == "symbol_":
		return node.getName()[7:]
	elif node.getName()[0:7] == "string_":
		return "\"" + node.getName()[7:] + "\""
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

def sendToReaderStack(readerStack, nextCharacter):
	print(nextCharacter)
	if len(readerStack) == 0:
		print("yo")
		RootReader(readerStack)
	root = readerStack[0].getValue()

	characterToProcess = nextCharacter
	while characterToProcess:
		print(characterToProcess)
		characterToProcess = readerStack[-1].readNextCharacter(readerStack, nextCharacter)
		if len(readerStack) == 0:
			assert(characterToProcess == nextCharacter)
			return root
def lisp():
	input = open("test1.cl", "r")
	readerStack = []
	nextCharacter = input.read(1)
	while nextCharacter:
		expression = sendToReaderStack(readerStack, nextCharacter)
		if expression:
			print(treeToString(expression))
		else:
			nextCharacter = input.read(1)

lisp()
