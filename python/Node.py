NIL = {"nil"}

class Node:
	def __init__(self, value = "", car = None, cdr = None, extra = None):
		self.value = value
		self.car = car
		self.cdr = cdr
		self.extra = extra
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
	def getExtra(self):
		return self.extra

def isCons(node):
	if node == NIL:
		return False
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
	if isSymbol(node, "1"):
		return True
	elif isSymbol(node, "2"):
		return True
	elif isSymbol(node, "3"):
		return True
	elif isSymbol(node, "4"):
		return True
	elif isSymbol(node, "5"):
		return True
	else:
		return False

def isFunctionPointer(node):
	if node == NIL:
		return False
	if node.getValue()[0:8] != "function":
		return False
	return True

def FunctionPointer(name, pointer):
	return Cons("function_" + name, None, None, pointer)

def getFunctionName(node):
	assert(isFunctionPointer(node))
	return node.getValue()


