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

def newReader(initialCharacter):
	if initialCharacter == "(":
		return ConsReader(initialCharacter)
	elif initialCharacter == "\"":
		return StringReader(initialCharacter)
	elif initialCharacter == "'":
		return QuoteReader(initialCharacter)
	elif initialCharacter == "`":
		return QuasiquoteReader(initialCharacter)
	elif initialCharacter == ",":
		return CommaReader(initialCharacter)
	else:
		return SymbolReader(initialCharacter)

class RootReader():
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
			self.childReader = newReader(nextCharacter)
			return

class SymbolReader():
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
			self.elementReader = newReader(nextCharacter)
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
	def __init__(self, initialCharacter):
		assert(initialCharacter == "'")
		self.reader = None
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if not self.reader:
			self.reader = newReader(nextCharacter)
		else:
			operand = self.reader.readNextCharacter(nextCharacter)
			if operand:
				self.done = True
				self.reader = None
				result = Cons(Symbol("quote"), Cons(operand, NIL))
				return result

class QuasiquoteReader():
	def __init__(self, initialCharacter):
		assert(initialCharacter == "`")
		self.reader = None
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if not self.reader:
			self.reader = newReader(nextCharacter)
		else:
			operand = self.reader.readNextCharacter(nextCharacter)
			if operand:
				self.done = True
				self.reader = None
				result = Cons(Symbol("quasiquote"), Cons(operand, NIL))
				return result

class CommaReader():
	def __init__(self, initialCharacter):
		assert(initialCharacter == ",")
		self.reader = None
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if not self.reader:
			self.reader = newReader(nextCharacter)
		else:
			operand = self.reader.readNextCharacter(nextCharacter)
			if operand:
				self.done = True
				self.reader = None
				result = Cons(Symbol("comma"), Cons(operand, NIL))
				return result

def add_n_quotes(consOrAtom, n):
	result = consOrAtom
	for quoteIdx in range(0, n):
		result = Cons(Symbol("quote"), result)
	return result

def list_append(cons, element):
	if not cons:
		return Cons(element, NIL)
	lastElement = cons
	while lastElement.getCdr() != NIL:
		lastElement = lastElement.getCdr()
	lastElement.setCdr(Cons(element, NIL))
	return cons

def expandBackquoteMacro(consOrAtom, backquoteLevel = 0):
	print("inside call")
	if consOrAtom.getType() == "cons":
		if consOrAtom.getCar().getValue() == "quasiquote":
			backquoteLevel += 1
			print("expanding quasiquote " + consOrAtom.getCdr().getCar().getValue())
			print(backquoteLevel)
			return expandBackquoteMacro(consOrAtom.getCdr().getCar(), backquoteLevel)
		elif consOrAtom.getCar().getValue() == "comma":
			assert(backquoteLevel > 0)
			backquoteLevel -= 1
			print("reducing comma " + consOrAtom.getCdr().getCar().getValue())
			print(backquoteLevel)
			return expandBackquoteMacro(consOrAtom.getCdr().getCar(), backquoteLevel)
		else:
			result = None
			for idx in range(0, backquoteLevel):
				result = list_append(result, Symbol("list"))
			currentCons = consOrAtom
			while currentCons != NIL:
				nextElement = currentCons.getCar()
				print("expanding next element " + treeToString(result))
				result = list_append(result, expandBackquoteMacro(nextElement, backquoteLevel))
				currentCons = currentCons.getCdr()
			return result
	else:
		return add_n_quotes(consOrAtom, backquoteLevel)

def treeToString(node, addParenthesis = True):
	if node.getType() == "cons":
		if node.getCar() != NIL:
			if node.getCar().getType() == "symbol":
				if node.getCar().getValue() == "quote":
					return "'" + treeToString(node.getCdr().getCar())
				elif node.getCar().getValue() == "quasiquote":
					return "`" + treeToString(node.getCdr().getCar())
				elif node.getCar().getValue() == "comma":
					return "," + treeToString(node.getCdr().getCar())
	if node.getType() == "cons":
		if node.getCdr() == NIL:
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

