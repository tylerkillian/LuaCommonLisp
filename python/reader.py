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
		return BackquoteReader(initialCharacter)
	elif initialCharacter == ",":
		return CommaReader(initialCharacter)
	else:
		return SymbolReader(initialCharacter)

class RootReader:
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

class SymbolReader:
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

class StringReader:
	def __init__(self, initialCharacter):
		assert(initialCharacter == "\"")
		self.buffer = ""
		self.mode = "readingString"
		self.done = False
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.mode == "waitingForTerminalCharacter":
			self.done = True
			return String(self.buffer)
		elif nextCharacter == "\"":
			self.mode = "waitingForTerminalCharacter"
			return
		else:
			self.buffer = self.buffer + nextCharacter
			return

class ConsReader:
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

def makeTwoElementList(itemOne, itemTwo):
	return Cons(itemOne, Cons(itemTwo))

class QuoteReader:
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
				result = makeTwoElementList(Symbol("quote"), operand)
				return result

class BackquoteReader:
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
				result = Cons(Symbol("backquote"), Cons(operand, NIL))
				return result

class CommaReader:
	def __init__(self, initialCharacter):
		assert(initialCharacter == ",")
		self.reader = None
		self.done = False
		self.readingSecondCharacter = True
		self.commaType = "comma"
	def readNextCharacter(self, nextCharacter):
		assert(not self.done)

		if self.readingSecondCharacter:
			self.readingSecondCharacter = False
			if nextCharacter == "@":
				self.commaType = "comma-at"
				return

		if not self.reader:
			self.reader = newReader(nextCharacter)
		else:
			operand = self.reader.readNextCharacter(nextCharacter)
			if operand:
				self.done = True
				self.reader = None
				result = Cons(Symbol(self.commaType), Cons(operand, NIL))
				return result

def list_getLength(cons):
	assert(isCons(cons))
	length = 0
	current = cons
	while current != NIL:
		current = current.getCdr()
		length += 1
	return length

def list_get(cons, index):
	assert(isCons(cons))
	count = 0
	current = cons
	while count < index:
		assert(isCons(cons))
		current = current.getCdr()
		count += 1
	assert(current)
	return current.getCar()

def list_append(cons, element):
	if not cons:
		return Cons(element, NIL)
	lastElement = cons
	while lastElement.getCdr() != NIL:
		lastElement = lastElement.getCdr()
	lastElement.setCdr(Cons(element, NIL))
	return cons

def getBackquoteDepth(expression, backquoteLevel = 0):
	if expression == NIL:
		return backquoteLevel
	elif isSymbol(expression) or isString(expression):
		return backquoteLevel
	elif isCons(expression):
		if isSymbol(expression.getCar(), "backquote"):
			backquoteLevel += 1
			return getBackquoteDepth(expression.getCdr(), backquoteLevel)
		else:
			carBackquoteDepth = getBackquoteDepth(expression.getCar(), backquoteLevel)
			cdrBackquoteDepth = getBackquoteDepth(expression.getCdr(), backquoteLevel)
			return max(carBackquoteDepth, cdrBackquoteDepth)
	else:
		assert(False)

def findFirstBackquoteAtGivenDepth(expression, depthToFind, currentDepth = 0):
	if expression == NIL:
		return None
	elif isCons(expression):
		if isSymbol(expression.getCar(), "backquote"):
			currentDepth += 1
			if currentDepth == depthToFind:
				return expression
			else:
				return findFirstBackquoteAtGivenDepth(expression.getCdr(), depthToFind, currentDepth)
		else:
			resultFromCar = findFirstBackquoteAtGivenDepth(expression.getCar(), depthToFind, currentDepth)
			if resultFromCar:
				return resultFromCar
			else:
				return findFirstBackquoteAtGivenDepth(expression.getCdr(), depthToFind, currentDepth)
	else:
		return None

def getInnerBackquote(expression):
	innerBackquoteDepth = getBackquoteDepth(expression)
	if innerBackquoteDepth == 0:
		return None
	else:
		return findFirstBackquoteAtGivenDepth(expression, innerBackquoteDepth)

def replaceFirstBackquoteAtGivenDepth(expression, replacement, depthToFind, currentDepth = 0):
	if isCons(expression):
		if isSymbol(expression.getCar(), "backquote"):
			currentDepth += 1
			if currentDepth == depthToFind:
				return replacement
			else:
				newCdr = replaceFirstBackquoteAtGivenDepth(expression.getCdr(), replacement, depthToFind, currentDepth)
				return Cons(expression.getCar(), newCdr)
		else:
			foundInCar = findFirstBackquoteAtGivenDepth(expression.getCar(), depthToFind, currentDepth)
			if foundInCar:
				newCar = replaceFirstBackquoteAtGivenDepth(expression.getCar(), replacement, depthToFind, currentDepth)
				return Cons(newCar, expression.getCdr())
			else:
				newCdr = replaceFirstBackquoteAtGivenDepth(expression.getCdr(), replacement, depthToFind, currentDepth)
				return Cons(expression.getCar(), newCdr)
	else:
		return expression

def replaceInnerBackquote(expression, replacement):
	innerBackquoteDepth = getBackquoteDepth(expression)
	if innerBackquoteDepth == 0:
		return expression
	else:
		return replaceFirstBackquoteAtGivenDepth(expression, replacement, innerBackquoteDepth)

def levelUp(element):
	if element == NIL:
		result = list_append(None, Symbol("list"))
		return list_append(result, makeTwoElementList(Symbol("quote"), NIL))
	elif isSymbol(element) or isString(element):
		result = list_append(None, Symbol("list"))
		return list_append(result, makeTwoElementList(Symbol("quote"), element))
	elif isCons(element):
		if isSymbol(element.getCar(), "comma"):
			result = list_append(None, Symbol("list"))
			return list_append(result, element.getCdr().getCar())
		elif isSymbol(element.getCar(), "comma-at"):
			return element.getCdr().getCar()
		else:
			result = list_append(None, Symbol("list"))
			adding = Cons(Symbol("backquote"), element)
			return list_append(result, makeTwoElementList(Symbol("backquote"), element))
			
def expandSingleBackquote(expression):
	assert(isSymbol(expression.getCar(), "backquote"))
	subexpression = expression.getCdr().getCar()
	if subexpression == NIL:
		return makeTwoElementList(Symbol("quote"), NIL)
	elif isSymbol(subexpression) or isString(subexpression):
		return makeTwoElementList(Symbol("quote"), subexpression)
	else:
		if isSymbol(subexpression.getCar(), "comma"):
			return subexpression.getCdr().getCar()
		else:
			result = list_append(None, Symbol("append"))
			for idx in range(0, list_getLength(subexpression)):
				element = list_get(subexpression, idx)
				result = list_append(result, levelUp(element))
			return result

def expandBackquoteMacro(expression):
	result = expression
	innerBackquote = getInnerBackquote(result)
	while innerBackquote:
		expandedInnerBackquote = expandSingleBackquote(innerBackquote)
		result = replaceInnerBackquote(result, expandedInnerBackquote)
		innerBackquote = getInnerBackquote(result)
	return result

def expressionToString(node, addParenthesis = True):
	if node == NIL:
		return "nil"
	elif isCons(node):
		if isSymbol(node.getCar(), "quote"):
			return "'" + expressionToString(node.getCdr().getCar())
		elif isSymbol(node.getCar(), "backquote"):
			return "`" + expressionToString(node.getCdr().getCar())
		elif isSymbol(node.getCar(), "comma"):
			return "," + expressionToString(node.getCdr().getCar())
		elif isSymbol(node.getCar(), "comma-at"):
			return ",@" + expressionToString(node.getCdr().getCar())
			
		if node.getCdr() == NIL:
			result = expressionToString(node.getCar())
		elif isCons(node.getCdr()):
			result = expressionToString(node.getCar()) + " " + expressionToString(node.getCdr(), False)
		else:
			result = expressionToString(node.getCar()) + " " + expressionToString(node.getCdr())
		if addParenthesis:
			result = "(" + result + ")" 
		return result
	elif isSymbol(node):
		return getSymbolValue(node)
	else:
		assert(isString(node))
		return '"' + getStringValue(node) + '"'

