NIL = {"nil"}

class Cons:
	def __init__(self, car = NIL, cdr = NIL):
		if type(car) == str:
			self.value = car
			self.car = None
			self.cdr = None
		else:
			self.value = "cons"
			self.car = car
			self.cdr = cdr
	def getType(self):
		if self.value == "cons":
			return "cons"
		elif self.value[0:6] == "symbol":
			return self.value[7:]
		else:
			assert(self.value[0:6] == "string")
			return self.value[7:]
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

def isSymbol(value, name = None):
	if value.getType() != "symbol":
		return False
	if name:
		if value.getValue() != name:
			return False
	return True

class Symbol:
	def __init__(self, value = ""):
		self.value = value
	def getType(self):
		return "symbol"
	def getValue(self):
		return self.value
	def setValue(self, value):
		self.value = value

class String:
	def __init__(self, value = ""):
		self.value = value
	def getType(self):
		return "string"
	def getValue(self):
		return self.value
	def setValue(self, value):
		self.value = value

