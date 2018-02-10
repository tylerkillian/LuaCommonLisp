class Node():
	def __init__(self, name, parent = None):
		self.name = name
		self.parent = parent
		self.children = []
	def getName(self):
		return self.name
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
	return SymbolReader(initialCharacter)

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

class ConsReader():
	def __init__(self, initialCharacter):
		self.reader = None
		self.previousCharacterWasDot = False
		self.gotDot = False
		self.value = Node("cons")
		self.terminateOnNextCharacter = False
		self.done = False
		if initialCharacter != "(":
			self.reader = newReader(initialCharacter)
	def sendToReader(self, nextCharacter):
		child = self.reader.readNextCharacter(nextCharacter)
		if child:
			self.reader = None
			self.value.addChild(child)
			return self.readNextCharacter(nextCharacter)
	def closeCons(self):
		assert(self.value.getNumChildren() <= 2)
		while self.value.getNumChildren() < 2:
			self.value.addChild(Node("nil"))
		self.terminateOnNextCharacter = True
	def readCharacterAfterDot(self, nextCharacter):
		assert(self.previousCharacterWasDot)
		self.previousCharacterWasDot = False
		if isWhitespace(nextCharacter):
			self.gotDot = True
		else:
			self.reader = newReader(".")
			shouldBeNull = self.reader.readNextCharacter(nextCharacter)
			assert(not shouldBeNull)
	def checkForCarOrCdr(self, nextCharacter):
		if not isWhitespace(nextCharacter):
			if self.value.getNumChildren() == 0:
				self.reader = newReader(nextCharacter)
			else:
				self.reader = ConsReader(nextCharacter)
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.terminateOnNextCharacter:
			self.done = True
			return self.value
		elif self.reader:
			return self.sendToReader(nextCharacter)
		elif nextCharacter == ")":
			assert(not self.previousCharacterWasDot)
			self.closeCons()
			return
		elif self.previousCharacterWasDot:
			return self.readCharacterAfterDot(nextCharacter)
		elif nextCharacter == ".":
			self.previousCharacterWasDot = True
			return
		else:
			return self.checkForCarOrCdr(nextCharacter)

class ConsReader2():
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
			return self.read(". " + nextCharacter)
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
	reader = ConsReader2("(")
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_nil")
	assert(result.getChild(1).getName() == "symbol_nil")
def test_ConsReader_readSingleElementList():
	reader = ConsReader2("(")
	result = reader.readNextCharacter("a")
	assert(not result)
	result = reader.readNextCharacter(")")
	assert(not result)
	result = reader.readNextCharacter(" ")
	assert(result)
	assert(result.getName() == "cons")
	assert(result.getNumChildren() == 2)
	assert(result.getChild(0).getName() == "symbol_a")
	assert(result.getChild(1).getName() == "nil")
def test_ConsReader_readConsWithDot():
	reader = ConsReader2("(")
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
	assert(result.getChild(1).getChild(1).getName() == "nil")
test_ConsReader = {
	"test_ConsReader_readEmptyList": test_ConsReader_readEmptyList,
	"test_ConsReader_readSingleElementList": test_ConsReader_readSingleElementList,
	"test_ConsReader_readConsWithDot": test_ConsReader_readConsWithDot,
	#"test_ConsReader_readTwoElementList": test_ConsReader_readTwoElementList,
}
runTests(test_ConsReader)
		




class Parser():
	def __init__(self):
		self.currentNode = Node("root")
		self.mode = "whitespace"
		self.previousCharacter = None
	def _addCons(self):
		self.currentNode.addChild(Node("cons"))
	def _whitespace(self, character):
		print(character)
		if isWhitespace(character):
			return
		elif character == "(":
			self._addCons()
	def nextCharacter(self, character):
		if self.mode == "whitespace":
			self._whitespace(character)

def parse(string):
	parser = Parser()
	for character in "(setf a 2)":
		result = parser.nextCharacter(character)
		if result:
			return result
	return

result = parse("(setf a 2)")
#assert(result.toString() == "(setf a 2)")

