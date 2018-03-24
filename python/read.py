from reader import RootReader

def read(inputStream):
	nextCharacter = inputStream.peekNextCharacter()
	reader = RootReader()
	while nextCharacter:
		result = reader.readNextCharacter(nextCharacter)
		if result:
			return result
		else:
			inputStream.popCharacter()
			nextCharacter = inputStream.peekNextCharacter()

