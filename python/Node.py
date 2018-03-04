class Node():
	def __init__(self, name, parent = None):
		self.name = name
		self.parent = parent
		self.children = []
		self.value = name[7:]
		self.car = None
		self.cdr = None
	def __repr__(self):
		return self.value
	def getName(self):
		return self.name
	def setName(self, name):
		self.name = name
	def getParent(self):
		return self.parent
	def setParent(self, parent):
		self.parent = parent
	def getNumChildren(self):
		return len(self.children)
	def addChild(self, child):
		self.children.append(child)
	def getChild(self, childIdx):
		assert(childIdx < len(self.children))
		return self.children[childIdx]
	def getType(self):
		if self.name[0:6] == "symbol":
			return "symbol"
		elif self.name[0:6] == "string":
			return "string"
		elif self.name == "cons":
			return "cons"
		else:
			assert(False)
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

class Cons():
	def __init__(self):
		self.name = "cons"
		self.children = []
		self.value = ""
		self.car = None
		self.cdr = None
	def __repr__(self):
		return self.value
	def getName(self):
		return self.name
	def setName(self, name):
		self.name = name
	def getParent(self):
		return self.parent
	def setParent(self, parent):
		self.parent = parent
	def getNumChildren(self):
		return len(self.children)
	def addChild(self, child):
		self.children.append(child)
	def getChild(self, childIdx):
		assert(childIdx < len(self.children))
		return self.children[childIdx]
	def getType(self):
		if self.name[0:6] == "symbol":
			return "symbol"
		elif self.name[0:6] == "string":
			return "string"
		elif self.name == "cons":
			return "cons"
		else:
			assert(False)
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

def Symbol(name = ""):
	return Node("symbol_" + name)

def String(value = ""):
	return Node("string_" + value)
