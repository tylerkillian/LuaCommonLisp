class Cons():
	def __init__(self):
		self.car = None
		self.cdr = None
	def getType(self):
		return "cons"
	def getCar(self):
		return self.car
	def setCar(self, car):
		self.car = car
	def getCdr(self):
		return self.cdr
	def setCdr(self, cdr):
		self.cdr = cdr

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
