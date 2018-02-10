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
		print("Testing " + testName)
		testFunction() 
runTests(test_Node)


def newReader(character):
	return SymbolReader(initialCharacter)

def SymbolReader():
	def __init__(self, initialCharacter):
		self.value = initialCharacter
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)
		if nextCharacter == " " or nextCharacter == ")":
			self.done = True
			return Node("symbol_" + self.value)
		else
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

def ConsReader():
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
	def readCharacterAfterDot(self, nextCharacter):
		if self.previousCharacterWasDot:
			self.previousCharacterWasDot = False
			if isWhitespace(nextCharacter):
				self.gotDot = True
				return
			else:
				self.reader = newReader(".")
				shouldBeNull = self.reader.readNextCharacter(nextCharacter)
				assert(not shouldBeNull)
				return
		else:
	def checkForCarOrDotOrCdr(self, nextCharacter):
		if self.previousCharacterWasDot:
			self.previousCharacterWasDot = False
			if isWhitespace(nextCharacter):
				self.gotDot = True
				return
			else:
				self.reader = newReader(".")
				shouldBeNull = self.reader.readNextCharacter(nextCharacter)
				assert(not shouldBeNull)
				return
		else:
			if nextCharacter == ".":
				self.previousCharacterWasDot = True
				return
			elif isWhitespace(nextCharacter):
				return
			else:
				self.reader = newReader(nextCharacter)
				return
	def closeCons(self):
		assert(self.value.getNumChildren() <= 2)
		while self.value.getNumChildren() < 2:
			self.value.addChild(Node("nil"))
		self.terminateOnNextCharacter = True
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
			return self.checkForCarOrDotOrCdr(nextCharacter)
		




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

