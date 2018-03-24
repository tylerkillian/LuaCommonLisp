class Stream:
	def __init__(self):
		self.values = []
	def push(self, string):
		for character in string:
			self.values.append(character)
	def popCharacter(self):
		if len(self.values) == 0:
			return
		else:
			return self.values.pop(0)
