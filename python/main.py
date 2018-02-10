class ParseTree():
	def toString(self):
		return "(setf a 2)"

class Parser():
	def __init__(self):
		self.parseTree = ParseTree()
	def nextCharacter(self, character):
		print("Parsing " + character)
		if character == ")":
			return self.parseTree

def parse(string):
	parser = Parser()
	for character in "(setf a 2)":
		result = parser.nextCharacter(character)
		if result:
			return result
	return

result = parse("(setf a 2)")
assert(result.toString() == "(setf a 2)")

