class Parser():
	def nextCharacter(character):
		print("Parsing " + character)


parser = Parser()
for character in "(setf a 2}":
	parser.nextCharacter(character)
