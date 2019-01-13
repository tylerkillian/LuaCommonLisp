from reader import RootReader

class LispFileReader:
	def __init__(self, inputFile):
		self.input = open(inputFile, "r")
		self.reader = RootReader()
	def read(self):
		nextCharacter = self.input.read(1)
		while nextCharacter:
			expression = self.reader.readNextCharacter(nextCharacter)
			if expression:
				self.reader = RootReader()
				return expression
			else:
				nextCharacter = self.input.read(1)

