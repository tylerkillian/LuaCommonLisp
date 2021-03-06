class Stream:
	def __init__(self, initialize = ""):
		self.values = []
		for value in initialize:
			self.values.append(value)
	def peekNextCharacter(self):
		if len(self.values) == 0:
			return
		else:
			return self.values[0]
	def readCharacter(self):
		if len(self.values) == 0:
			return
		else:
			return self.values.pop(0)
	def read(self):
		result = ""
		nextCharacter = self.readCharacter()
		while nextCharacter:
			result += nextCharacter
			nextCharacter = self.readCharacter()
		return result
	def write(self, string):
		for character in string:
			self.values.append(character)
	def isEmpty(self):
		if len(self.values) == 0:
			return True
		else:
			return False
