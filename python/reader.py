from Node import Node, Cons, Symbol, String

def newReader(readerStack, initialCharacter, parentNode):
	if initialCharacter == "(":
		return ConsReader(readerStack, initialCharacter, parentNode)
	elif initialCharacter == "\"":
		return StringReader(readerStack, initialCharacter, parentNode)
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
			consReader = newReader(readerStack, nextCharacter, self.value)
			self.value.addChild(consReader.getValue())
			return


class SymbolReader():
	def __init__(self, readerStack, initialCharacter, parentNode = None):
		readerStack.append(self)
		self.value = Node("symbol_" + initialCharacter, parentNode)
		self.value2 = Symbol(initialCharacter)
		self.done = False
	def getValue(self):
		return self.value
	def getValue2(self):
		return self.value2
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		if nextCharacter == " " or nextCharacter == ")" or nextCharacter == "\n":
			self.done = True
			readerStack.pop()
			return nextCharacter
		else:
			self.value.setName(self.value.getName() + nextCharacter)
			self.value2.setValue(self.value2.getValue() + nextCharacter)
			return

class StringReader():
	def __init__(self, readerStack, initialCharacter, parentNode = None):
		assert(initialCharacter == "\"")
		readerStack.append(self)
		self.value = Node("string_" + initialCharacter, parentNode)
		self.value2 = String(initialCharacter)
		self.mode = "readingString"
		self.done = False
	def getValue(self):
		return self.value
	def getValue2(self):
		return self.value2
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
			self.value.setName(self.value.getName() + nextCharacter)
			self.value2.setValue(self.value2.getValue() + nextCharacter)
			self.mode = "waitingForTerminalCharacter"
			return
		else:
			self.value.setName(self.value.getName() + nextCharacter)
			self.value2.setValue(self.value2.getValue() + nextCharacter)
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
		self.value2 = Cons()
		self.done = False
	def getValue(self):
		return self.value
	def getValue2(self):
		return self.value2
	def isDone(self):
		return self.done
	def processStage_waitingForCar(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForCar")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			self.value2.setCar(None)
			self.value2.setCdr(None)
			self.value.addChild(Node("symbol_nil", self.value))
			self.value.addChild(Node("symbol_nil", self.value))
			assert(self.value.getNumChildren() == 2)
			self.stage = "waitingForTerminalCharacter"
		else:
			self.stage = "waitingForDot"
			readCar = newReader(readerStack, nextCharacter, self.value)
			self.value.addChild(readCar.getValue())
			self.value2.setCar(readCar.getValue2())
			assert(self.value.getNumChildren() == 1)
	def beginReadingNextListElement(self, readerStack, characters):
		self.done = True
		readerStack.pop()
		readNextListElement = ConsReader(readerStack, "(", self.value)
		self.value.addChild(readNextListElement.getValue())
		self.value2.setCdr(readNextListElement.getValue2())
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
			self.value2.setCdr(None)
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
		return node.getName()[7:]
