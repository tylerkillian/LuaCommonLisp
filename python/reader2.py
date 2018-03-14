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

def newReader(readerStack, initialCharacter):
        if initialCharacter == "(":
                return ConsReader(readerStack, initialCharacter)
        elif initialCharacter == "\"":
                return StringReader(readerStack, initialCharacter)
        elif initialCharacter == "'":
                return QuoteReader(readerStack, initialCharacter)
        elif initialCharacter == "`":
                return QuasiquoteReader(readerStack, initialCharacter)
        elif initialCharacter == ",":
                return CommaReader(readerStack, initialCharacter)
        else:
                return SymbolReader(readerStack, initialCharacter)

def newReader2(initialCharacter):
	if initialCharacter == "(":
		return ConsReader2(initialCharacter)
	elif initialCharacter == "\"":
		return StringReader2(initialCharacter)
	elif initialCharacter == "'":
		return QuoteReader2(initialCharacter)
	elif initialCharacter == "`":
		return QuasiquoteReader2(initialCharacter)
	elif initialCharacter == ",":
		return CommaReader2(initialCharacter)
	else:
		return SymbolReader2(initialCharacter)

class RootReader2():
	def __init__(self):
		self.childReader = None
		self.done = False
	def isDone(self):
		return self.done
	def readNextCharacter(self, readerStack, nextCharacter):
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

class SymbolReader():
	def __init__(self, readerStack, initialCharacter):
		readerStack.append(self)
		self.value = Symbol(initialCharacter)
		self.done = False
	def getValue(self):
		return self.value
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
			self.value.setValue(self.value.getValue() + nextCharacter)
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

class StringReader():
	def __init__(self, readerStack, initialCharacter):
		assert(initialCharacter == "\"")
		readerStack.append(self)
		self.value = String(initialCharacter)
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
			self.value.setValue(self.value.getValue() + nextCharacter)
			self.mode = "waitingForTerminalCharacter"
			return
		else:
			self.value.setValue(self.value.getValue() + nextCharacter)
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


class ConsReader():
	def __init__(self, readerStack, initialCharacter):
		assert(initialCharacter == "(")
		readerStack.append(self)
		self.stage = "waitingForCar"
		self.value = Cons()
		self.done = False
	def getValue(self):
		return self.value
	def isDone(self):
		return self.done
	def processStage_waitingForCar(self, readerStack, nextCharacter):
		assert(self.stage == "waitingForCar")
		if isWhitespace(nextCharacter):
			return
		elif nextCharacter == ")":
			self.value.setCar(None)
			self.value.setCdr(None)
			self.stage = "waitingForTerminalCharacter"
		else:
			self.stage = "waitingForDot"
			readCar = newReader(readerStack, nextCharacter)
			self.value.setCar(readCar.getValue())
	def beginReadingNextListElement(self, readerStack, characters):
		self.done = True
		readerStack.pop()
		readNextListElement = ConsReader(readerStack, "(")
		self.value.setCdr(readNextListElement.getValue())
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
			self.value.setCdr(None)
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
			cdrReader = newReader(readerStack, nextCharacter)
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
			lastCdr = None

		if len(self.elements) == 0:
			return None

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

def treeToString(node, addParenthesis = True):
	if node.getType() == "cons":
		if node.getCdr() == None:
			result = treeToString(node.getCar())
		elif node.getCdr().getType() == "cons":
			result = treeToString(node.getCar()) + " " + treeToString(node.getCdr(), False)
		else:
			result = treeToString(node.getCar()) + " " + treeToString(node.getCdr())
		if addParenthesis:
			result = "(" + result + ")" 
		return result
	elif node.getType() == "symbol":
		return node.getValue()
	elif node.getType() == "string":
		return node.getValue()
	elif node.getType() == "quote":
		return "'" + treeToString(node.getOperand())
	elif node.getType() == "quasiquote":
		return "`" + treeToString(node.getOperand())
	elif node.getType() == "comma":
		return "," + treeToString(node.getOperand())
	else:
		assert(False)

