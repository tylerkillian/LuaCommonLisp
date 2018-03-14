from Node import *

def isWhitespace(character):
	if character == " ":
		return True
	elif character == "\t":
		return True
	elif character == "\n":
		return True
	else:
		return False

def newReader2(initialCharacter):
	if initialCharacter == "(":
		return ConsReader2(initialCharacter)
	elif initialCharacter == "\"":
		return StringReader2(initialCharacter)
	elif initialCharacter == "'":
		return QuoteReader2(initialCharacter)
	elif initialCharacter == "`":
		return QuasiquoteReader(initialCharacter)
	elif initialCharacter == ",":
		return CommaReader(initialCharacter)
	else:
		return SymbolReader2(initialCharacter)

class RootReader2():
	def __init__(self):
		self.childReader = None
		self.done = False
	def isDone(self):
		return self.done
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.childReader:
			result = self.childReader.readNextCharacter(nextCharacter)
			if result:
				self.done = True
				self.childReader = None
				return result
			return
		elif not isWhitespace(nextCharacter):
			self.childReader = newReader2(nextCharacter)
			return

class SymbolReader2():
	def __init__(self, initialCharacter):
		self.buffer = initialCharacter
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if nextCharacter == " " or nextCharacter == ")" or nextCharacter == "\n":
			self.done = True
			return Symbol(self.buffer)
		else:
			self.buffer = self.buffer + nextCharacter
			return

class StringReader2():
	def __init__(self, initialCharacter):
		assert(initialCharacter == "\"")
		self.buffer = initialCharacter
		self.mode = "readingString"
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.mode == "waitingForTerminalCharacter":
			self.done = True
			return String(self.buffer)
		elif nextCharacter == "\"":
			self.buffer = self.buffer + nextCharacter
			self.mode = "waitingForTerminalCharacter"
			return
		else:
			self.buffer = self.buffer + nextCharacter
			return

class ConsReader2():
	def __init__(self, initialCharacter):
		assert(initialCharacter == "(")
		self.stage = "waitingForElement"
		self.elements = []
		self.isDotted = False
		self.done = False
		self.elementReader = None
	def processStage_waitingForElement(self, nextCharacter):
		assert(self.stage == "waitingForElement")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ".":
			assert(len(self.elements) > 0)
			assert(not self.isDotted)
			self.isDotted = True
			self.stage = "readingDot"
			return
		elif nextCharacter == ")":
			self.stage = "waitingForTerminalCharacter"
			return
		else:
			self.stage = "readingElement"
			self.elementReader = newReader2(nextCharacter)
			return
	def processStage_readingElement(self, nextCharacter):
		assert(self.stage == "readingElement")
		assert(self.elementReader)
		element = self.elementReader.readNextCharacter(nextCharacter)
		if element:
			self.elements.append(element)
			self.elementReader = None
			if nextCharacter == ")":
				self.stage = "waitingForTerminalCharacter"
				return
			else:
				self.stage = "waitingForElement"
	def processStage_readingDot(self, nextCharacter):
		assert(self.stage == "readingDot")
		assert(isWhitespace(nextCharacter))
		self.stage = "waitingForElement"
	def processStage_waitingForTerminalCharacter(self, nextCharacter):
		assert(self.stage == "waitingForTerminalCharacter")
		self.done = True

		if self.isDotted:
			assert(len(self.elements) >= 2)
			cars = self.elements[0:-1]
			lastCdr = self.elements[len(self.elements)-1]
		else:
			cars = self.elements
			lastCdr = NIL

		if len(self.elements) == 0:
			return NIL

		result = Cons()
		currentCons = result
		for carIdx in range(0, len(cars)-1):
			currentCons.setCar(cars[carIdx])
			currentCons.setCdr(Cons())
			currentCons = currentCons.getCdr()
		currentCons.setCar(cars[-1])
		currentCons.setCdr(lastCdr)
		return result
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.stage == "waitingForElement":
			return self.processStage_waitingForElement(nextCharacter)
		elif self.stage == "readingElement":
			return self.processStage_readingElement(nextCharacter)
		elif self.stage == "readingDot":
			return self.processStage_readingDot(nextCharacter)
		else:
			assert(self.stage == "waitingForTerminalCharacter")
			return self.processStage_waitingForTerminalCharacter(nextCharacter)

class QuoteReader():
	def __init__(self, readerStack, initialCharacter):
		assert(initialCharacter == "'")
		readerStack.append(self)
		self.value = Quote()
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		self.done = True
		readerStack.pop()
		nextReader = newReader(readerStack, nextCharacter)
		self.value.setOperand(nextReader.getValue())

class QuoteReader2():
	def __init__(self, initialCharacter):
		assert(initialCharacter == "'")
		self.reader = None
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if not self.reader:
			self.reader = newReader2(nextCharacter)
		else:
			operand = self.reader.readNextCharacter(nextCharacter)
			if operand:
				self.done = True
				self.reader = None
				result = Cons(Symbol("quote"), Cons(operand, NIL))
				return result

class QuasiquoteReader():
	def __init__(self, readerStack, initialCharacter):
		assert(initialCharacter == "`")
		readerStack.append(self)
		self.value = Quasiquote()
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		self.done = True
		readerStack.pop()
		nextReader = newReader(readerStack, nextCharacter)
		self.value.setOperand(nextReader.getValue())

class CommaReader():
	def __init__(self, readerStack, initialCharacter):
		assert(initialCharacter == ",")
		readerStack.append(self)
		self.value = Comma()
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
		assert(not self.done)
		assert(readerStack[-1] == self)

		self.done = True
		readerStack.pop()
		nextReader = newReader(readerStack, nextCharacter)
		self.value.setOperand(nextReader.getValue())

def treeToString2(node, addParenthesis = True):
	if node.getType() == "cons":
		if node.getCdr() == NIL:
			result = treeToString2(node.getCar())
		elif node.getCdr().getType() == "cons":
			result = treeToString2(node.getCar()) + " " + treeToString2(node.getCdr(), False)
		else:
			result = treeToString2(node.getCar()) + " " + treeToString2(node.getCdr())
		if addParenthesis:
			result = "(" + result + ")" 
		return result
	elif node.getType() == "symbol":
		return node.getValue()
	elif node.getType() == "string":
		return node.getValue()
	elif node.getType() == "quote":
		return "'" + treeToString2(node.getOperand())
	elif node.getType() == "quasiquote":
		return "`" + treeToString2(node.getOperand())
	elif node.getType() == "comma":
		return "," + treeToString2(node.getOperand())
	else:
		assert(False)

