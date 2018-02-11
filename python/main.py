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


def newReader(initialCharacter):
	if initialCharacter == "(":
		return ConsReader(initialCharacter)
	else:
		return SymbolReader(initialCharacter)

def newReader2(readerStack, initialCharacter, parentNode):
	if initialCharacter == "(":
		return ConsReader2(readerStack, initialCharacter, parentNode)
	else:
		return SymbolReader2(readerStack, initialCharacter, parentNode)

class SymbolReader2():
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

class SymbolReader():
	def __init__(self, initialCharacter):
		self.value = initialCharacter
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)
		if nextCharacter == " " or nextCharacter == ")":
			self.done = True
			return Node("symbol_" + self.value)
		else:
			self.value += nextCharacter
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

class ConsReader2():
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
			readCar = newReader2(readerStack, nextCharacter, self.value)
			self.value.addChild(readCar.getValue())
			assert(self.value.getNumChildren() == 1)
	def beginReadingNextListElement(self, readerStack, characters):
		self.done = True
		readerStack.pop()
		readNextListElement = ConsReader2(readerStack, "(", self.value)
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
			cdrReader = newReader2(readerStack, nextCharacter, self.value)
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

def treeToString(node):
	if node.getName() == "root":
		return treeToString(node.getChild(0))
	elif node.getName() == "cons":
		return "(" + treeToString(node.getChild(0)) + " . " + treeToString(node.getChild(1)) + ")"
	elif node.getName()[0:7] == "symbol_":
		return node.getName()[7:]
def test_ConsReader2_emptyList():
	root = Node("root")
	readerStack = []

	ConsReader2(readerStack, "(", root)
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
def test_ConsReader2_singleElementList():
	root = Node("root")
	readerStack = []

	ConsReader2(readerStack, "(", root)
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
	newReader2(readerStack, string[0], root)
	assert(len(readerStack) == 1)
	root.addChild(readerStack[-1].getValue())

	for character in string:
		characterUsed = False
		while not characterUsed:
			lastResult = readerStack[-1].readNextCharacter(readerStack, character)
			if len(readerStack) == 0:
				assert(character == string[-1])
				assert(character == lastResult)
				return root
			if lastResult != character:
				characterUsed = True
	assert(not lastResult)
	return
def test_ConsReader2_twoElementList():
	root = parseString("(a b) ")

	assert(root.getNumChildren() == 1)
	assert(root.getChild(0).getName() == "cons")
	assert(root.getChild(0).getNumChildren() == 2)
	assert(root.getChild(0).getChild(0).getName() == "symbol_a")
	assert(root.getChild(0).getChild(1).getName() == "cons")
	assert(root.getChild(0).getChild(1).getNumChildren() == 2)
	assert(root.getChild(0).getChild(1).getChild(0).getName() == "symbol_b")
	assert(root.getChild(0).getChild(1).getChild(1).getName() == "symbol_nil")
test_ConsReader2 = {
	"test_ConsReader2_emptyList": test_ConsReader2_emptyList,
	"test_ConsReader2_singleElementList": test_ConsReader2_singleElementList,
	"test_ConsReader2_twoElementList": test_ConsReader2_twoElementList,
}
runTests(test_ConsReader2)



class ConsReader():
	def __init__(self, initialCharacter):
		self.stage = "waitingForCar"
		self.reader = None
		self.value = Node("cons")
		self.done = False
		self.isReadingList = False
		if initialCharacter != "(":
			self.reader = newReader(initialCharacter)
	def read(self, string):
		for characterIdx in range(0, len(string)):
			character = string[characterIdx]
			result = self.readNextCharacter(character)
			if characterIdx < len(string) - 1:
				assert(not result)
		return result
	def processStage_waitingForCar(self, nextCharacter):
		assert(self.stage == "waitingForCar")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			return self.read("nil . nil )")
		else:
			assert(self.reader == None)
			self.reader = newReader(nextCharacter)
			self.stage = "readingCar"
			return
	def processStage_readingCar(self, nextCharacter):
		assert(self.stage == "readingCar")
		assert(self.reader)
		child = self.reader.readNextCharacter(nextCharacter)
		if child:
			self.reader = None
			assert(self.value.getNumChildren() == 0)
			self.value.addChild(child)
			self.stage = "waitingForDot"
			return self.readNextCharacter(nextCharacter)
	def processStage_waitingForDot(self, nextCharacter):
		assert(self.stage == "waitingForDot")
		if nextCharacter == ".":
			self.stage = "readingDot"
			return
		elif isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			return self.read(". nil)")
		else:
			self.isReadingList = True
			return self.read(". (" + nextCharacter)
	def processStage_readingDot(self, nextCharacter):
		assert(self.stage == "readingDot")
		if isWhitespace(nextCharacter):
			self.stage = "waitingForCdr"
			return
		else:
			self.isReadingList = True
			return self.read(" ." + nextCharacter)
	def processStage_waitingForCdr(self, nextCharacter):
		assert(self.stage == "waitingForCdr")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			return self.read("nil )")
		else:
			assert(self.reader == None)
			self.reader = newReader(nextCharacter)
			self.stage = "readingCdr"
			return
	def processStage_readingCdr(self, nextCharacter):
		assert(self.stage == "readingCdr")
		assert(self.reader)
		child = self.reader.readNextCharacter(nextCharacter)
		if child:
			self.reader = None
			assert(self.value.getNumChildren() == 1)
			self.value.addChild(child)
			self.stage = "waitingForParentheses"
			if self.isReadingList:
				return self.read(" )" + nextCharacter)
			else:
				return self.readNextCharacter(nextCharacter)
	def processStage_waitingForParentheses(self, nextCharacter):
		assert(self.stage == "waitingForParentheses")
		if nextCharacter == ")":
			self.stage = "waitingForTerminalCharacter"
		else:
			assert(isWhitespace(nextCharacter))
	def processStage_waitingForTerminalCharacter(self, nextCharacter):
		assert(self.stage == "waitingForTerminalCharacter")
		self.done = True
		return self.value
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.stage == "waitingForCar":
			return self.processStage_waitingForCar(nextCharacter)
		elif self.stage == "readingCar":
			return self.processStage_readingCar(nextCharacter)
		elif self.stage == "waitingForDot":
			return self.processStage_waitingForDot(nextCharacter)
		elif self.stage == "readingDot":
			return self.processStage_readingDot(nextCharacter)
		elif self.stage == "waitingForCdr":
			return self.processStage_waitingForCdr(nextCharacter)
		elif self.stage == "readingCdr":
			return self.processStage_readingCdr(nextCharacter)
		elif self.stage == "waitingForParentheses":
			return self.processStage_waitingForParentheses(nextCharacter)
		else:
			assert(self.stage == "waitingForTerminalCharacter")
			return self.processStage_waitingForTerminalCharacter(nextCharacter)


def test_ConsReader_readEmptyList():
	reader = ConsReader("(")
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_nil")
	assert(result.getChild(1).getName() == "symbol_nil")
def test_ConsReader_readSingleElementList():
	reader = ConsReader("(")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_a")
	assert(result.getChild(1).getName() == "symbol_nil")
def test_ConsReader_readConsWithDot():
	reader = ConsReader("(")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(not result)
	result = reader.readNextCharacter(".")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(not result)
	result = reader.readNextCharacter("b")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(not result)
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_a")
	assert(result.getChild(1).getName() == "symbol_b")
def test_ConsReader_readTwoElementList():
	reader = ConsReader("(")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(not result)
	result = reader.readNextCharacter("b")
	assert(not result)
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_a")
	assert(result.getChild(1).getName() == "cons")
	assert(result.getChild(1).getChild(0).getName() == "symbol_b")
	assert(result.getChild(1).getChild(1).getName() == "symbol_nil")
test_ConsReader = {
	"test_ConsReader_readEmptyList": test_ConsReader_readEmptyList,
	"test_ConsReader_readSingleElementList": test_ConsReader_readSingleElementList,
	"test_ConsReader_readConsWithDot": test_ConsReader_readConsWithDot,
	"test_ConsReader_readTwoElementList": test_ConsReader_readTwoElementList,
}
runTests(test_ConsReader)
