from read import *
from reader import treeToString
from Stream import Stream
from Node import isSymbol, NIL

def test_readSymbol():
	inputStream = Stream("a ")
	symbol = read(inputStream)
	assert(isSymbol(symbol, "a"))

def test_readEmptyParentheses():
	inputStream = Stream("() ")
	expression = read(inputStream)
	assert(expression == NIL)
