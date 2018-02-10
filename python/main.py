class Parser():
	def nextCharacter(self, character):
		print("Parsing " + character)


parser = Parser()
for character in "(setf a 2)":
	parser.nextCharacter(character)
