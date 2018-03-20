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

def makeTwoElementList(itemOne, itemTwo):
	return Cons(itemOne, Cons(itemTwo))

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
				result = makeTwoElementList(Symbol("quote"), operand)
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
        assert(cons.getType() == "cons")
        length = 0
        current = cons
        while current != NIL:
                current = current.getCdr()
                length += 1
        return length

def list_get(cons, index):
        assert(cons.getType() == "cons")
        count = 0
        current = cons
        while count < index:
                assert(current.getType() == "cons")
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

def isSymbol(expression, name):
	if expression == NIL:
		return False
	elif expression.getType() == "symbol":
		if expression.getValue() == name:
			return True
	else:
		return False

def getBackquoteDepth(expression, backquoteLevel = 0):
	if expression == NIL:
		return backquoteLevel
	elif expression.getType() == "symbol" or expression.getType() == "string":
		return backquoteLevel
	elif expression.getType() == "cons":
		if isSymbol(expression.getCar(), "quasiquote"):
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
	elif expression.getType() == "cons":
		if isSymbol(expression.getCar(), "quasiquote"):
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
	if expression.getType() == "cons":
		if isSymbol(expression.getCar(), "quasiquote"):
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

def levelup(element):
	print("      element = " + treeToString(element))
	if element == NIL:
		result = list_append(None, Symbol("list"))
		return list_append(result, makeTwoElementList(Symbol("quote"), NIL))
	elif element.getType() == "symbol" or element.getType() == "string":
		result = list_append(None, Symbol("list"))
		return list_append(result, makeTwoElementList(Symbol("quote"), element))
	elif element.getType() == "cons":
		if element.getCar().getValue() == "comma":
			result = list_append(None, Symbol("list"))
			return list_append(result, element.getCdr().getCar())
		elif element.getCar().getValue() == "comma-at":
			return element.getCdr().getCar()
		else:
			result = list_append(None, Symbol("list"))
			adding = Cons(Symbol("quasiquote"), element)
			print("        adding " + treeToString(adding))
			return list_append(result, makeTwoElementList(Symbol("quasiquote"), element))
			
def expandSingleBackquote(expression):
	assert(expression.getCar().getValue() == "quasiquote")
	subexpression = expression.getCdr().getCar()
	print("  subexpression = " + treeToString(subexpression))
	if subexpression == NIL:
		return makeTwoElementList(Symbol("quote"), NIL)
	elif subexpression.getType() == "symbol" or subexpression.getType() == "string":
		return makeTwoElementList(Symbol("quote"), subexpression)
	else:
		if subexpression.getCar().getValue() == "comma":
			return subexpression.getCdr().getCar()
		else:
			print("  building")
			result = list_append(None, Symbol("append"))
			print("    " + treeToString(result))
			for idx in range(0, list_getLength(subexpression)):
				element = list_get(subexpression, idx)
				result = list_append(result, levelup(element))
				print("    " + treeToString(result))
			return result

def expandBackquoteMacro(expression):
	result = expression
	innerBackquote = getInnerBackquote(result)
	while innerBackquote:
		print("innerbackquote = " + treeToString(innerBackquote))
		expandedInnerBackquote = expandSingleBackquote(innerBackquote)
		print("replacing with " + treeToString(expandedInnerBackquote))
		result = replaceInnerBackquote(result, expandedInnerBackquote)
		innerBackquote = getInnerBackquote(result)
	return result

def treeToString(node, addParenthesis = True):
	if node.getType() == "cons":
		if node.getCar().getType() == "symbol":
			if node.getCar().getValue() == "quote":
				return "'" + treeToString(node.getCdr().getCar())
			elif node.getCar().getValue() == "quasiquote":
				return "`" + treeToString(node.getCdr().getCar())
			elif node.getCar().getValue() == "comma":
				return "," + treeToString(node.getCdr().getCar())
			elif node.getCar().getValue() == "comma-at":
				return ",@" + treeToString(node.getCdr().getCar())
			
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
	else:
		assert(False)

