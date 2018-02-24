from Node import *
from test_utilities import *

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
runTests(test_Node)

