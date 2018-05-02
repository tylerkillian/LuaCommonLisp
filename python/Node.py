NIL = {"nil"}

class Node:
	def __init__(self, value = "", car = None, cdr = None):
		self.value = value
		self.car = car
		self.cdr = cdr
	def getValue(self):
		return self.value
	def setValue(self, value):
		self.value = value
	def getCar(self):
		return self.car
	def setCar(self, car):
		self.car = car
	def getCdr(self):
		return self.cdr
	def setCdr(self, cdr):
		self.cdr = cdr

def isCons(node):
	if node == NIL:
		return True
	if node.getValue() == "cons":
		return True
	return False

def Cons(car = NIL, cdr = NIL):
	return Node("cons", car, cdr)

def isSymbol(node, value = None):
	if node == NIL:
		return False
	if node.getValue()[0:6] != "symbol":
		return False
	if value:
		if node.getValue()[7:] != value:
			return False
	return True

def Symbol(value = ""):
	return Node("symbol_" + value)

def getSymbolValue(node):
	assert(isSymbol(node))
	return node.getValue()[7:]

def Number(value = 0):
	return Symbol(str(value))

def isString(node, value = None):
	if node == NIL:
		return False
	if node.getValue()[0:6] != "string":
		return False
	if value:
		if node.getValue()[7:] != value:
			return False
	return True

def String(value = ""):
	return Node("string_" + value)

def getStringValue(node):
	assert(isString(node))
	return node.getValue()[7:]

def isNumber(node):
	if isSymbol(node):
		try:
			float(getSymbolValue(node))
			return True
		except:
			return False
	else:
		return False

def getNumberValue(node):
	assert(isSymbol(node))
	try:
		value = float(getSymbolValue(node))
		return value
	except:
		assert(False)

def isFunctionPointer(node):
	if node == NIL:
		return False
	if node.getValue()[0:8] != "function":
		return False
	return True

def FunctionPointer(name):
	return Node("function_" + name, None, None)

def getFunctionName(node):
	assert(isFunctionPointer(node))
	return node.getValue()[9:]

def Expression(*values):
	if len(values) == 0:
		return NIL

	previousCons = None
	for nextValue in values:
		nextCons = Cons(nextValue, NIL)
		if previousCons:
			previousCons.setCdr(nextCons)
		else:
			firstCons = nextCons
		previousCons = nextCons
	return firstCons


