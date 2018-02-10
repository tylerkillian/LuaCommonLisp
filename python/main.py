class Node():
	def __init__(self, name, parent = None):
		self.name = name
		self.parent = parent
		self.children = []
	def getName(self):
		return self.name
	def getParent(self):
		return self.parent
	def getNumChildren(self):
		return len(self.children)
	def addChild(self, child):
		self.children.append(child)
	def getChild(self, childIdx):
		assert(childIdx < len(self.children))
		return self.children[childIdx]

def test_Node_constructNoParent():
	node = Node("root")
	assert(node.getName() == "root")
	assert(node.getParent() == None)
	assert(node.getNumChildren() == 0)
def test_Node_constructWithParent():
	node = Node("theChild", Node("theParent"))
	assert(node.getParent().getName() == "theParent")
def test_Node_getName():
	node = Node("root")
	assert(node.getName() == "root")
def test_Node_addChild():
	node = Node("root")
	node.addChild(Node("child0"))
	assert(node.getNumChildren() == 1)
	assert(node.getChild(0).getName() == "child0")
test_Node = {
	"test_Node_constructNoParent": test_Node_constructNoParent,
	"test_Node_constructWithParent": test_Node_constructWithParent,
	"test_Node_getName": test_Node_getName,
	"test_Node_addChild": test_Node_addChild,
}
def runTests(tests):
	for testName, testFunction in tests.items():
		print("Testing " + testName)
		testFunction() 
runTests(test_Node)





def isWhitespace(character):
	if character == " ":
		return True
	elif character == "\t":
		return True
	elif character == "\n":
		return True
	else
		return False

class Parser():
	def __init__(self):
		self.currentNode = Node("root")
		self.mode = "whitespace"
	def nextCharacter(self, character):
		if isWhitespace(character):
			return
		elif charace

def parse(string):
	parser = Parser()
	for character in "(setf a 2)":
		result = parser.nextCharacter(character)
		if result:
			return result
	return

result = parse("(setf a 2)")
assert(result.toString() == "(setf a 2)")

