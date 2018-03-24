NIL = {"nil"}

class Node:
	def __init__(self, value = "", car = None, cdr = None):
		self.value = value
		self.car = car
		self.cdr = cdr
	def getType(self):
		if self.value == "cons":
			return "cons"
		elif self.value[0:6] == "symbol":
			return "symbol"
		else:
			assert(self.value[0:6] == "string")
			return "string"
	def getValue(self):
		if self.value == "cons":
			return "cons"
		elif self.value[0:6] == "symbol":
			return self.value[7:]
		else:
			assert(self.value[0:6] == "string")
			return self.value[7:]
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
	if node.getValue() == "cons":
		return True
	else:
		return False

def Cons(car = NIL, cdr = NIL):
	return Node("cons", car, cdr)

def isSymbol(node, value = None):
	if node.getValue()[0:6] != "symbol":
		return False
	if value:
		if node.getValue()[7:] != value:
			return False
	return True

def Symbol(value = ""):
	return Node("symbol_" + value)

def isString(node, value = None):
	if node.getValue()[0:6] != "string":
		return False
	if value:
		if node.getValue()[7:] != value:
			return False
	return True

def String(value = ""):
	return Node("string_" + value)

