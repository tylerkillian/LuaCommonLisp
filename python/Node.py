NIL = {"nil"}

class Cons():
	def __init__(self, car = NIL, cdr = NIL):
		self.car = car
		self.cdr = cdr
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

class Quote:
	def __init__(self, operand = None):
		self.operand = operand
	def getType(self):
		return "quote"
	def getOperand(self):
		return self.operand
	def setOperand(self, operand):
		self.operand = operand

class Quasiquote:
	def __init__(self):
		self.operand = None
	def getType(self):
		return "quasiquote"
	def getOperand(self):
		return self.operand
	def setOperand(self, operand):
		self.operand = operand

class Comma:
	def __init__(self):
		self.operand = None
	def getType(self):
		return "comma"
	def getOperand(self):
		return self.operand
	def setOperand(self, operand):
		self.operand = operand

