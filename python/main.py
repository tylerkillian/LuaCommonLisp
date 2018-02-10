class Node():
	def __init__(self, name):
		self.name = name
		self.parent = None
		self.children = []
	def getName(self):
		return self.name
	def addChild(child):
		self.children.append(child)

#@addTest(test_Node)
def test_Node_getName():
	node = Node("root")
	assert(node.getName() == "root")
test_Node = {
	"getName": test_Node_getName,
}
def runTests(tests):
	for testName, testFunction in tests.items():
		print("Testing " + testName)
		testFunction() 
runTests(test_Node)







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

