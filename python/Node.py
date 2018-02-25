class Node():
	def __init__(self, name, parent = None):
		self.name = name
		self.parent = parent
		self.children = []
	def getName(self):
		return self.name
	def setName(self, name):
		self.name = name
	def getParent(self):
		return self.parent
	def setParent(self, parent):
		self.parent = parent
	def getNumChildren(self):
		return len(self.children)
	def addChild(self, child):
		self.children.append(child)
	def getChild(self, childIdx):
		assert(childIdx < len(self.children))
		return self.children[childIdx]

