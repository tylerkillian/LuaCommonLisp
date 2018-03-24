from read import *
from reader import expressionToString
from Stream import Stream
from Node import isSymbol, NIL

def assertExpressionRead(inputString, result):
	inputStream = Stream(inputString)
	expression = read(inputStream)
	assert(expressionToString(expression) == result)

def test_readSymbol():
	assertExpressionRead("a ", "a")

def test_readQuotedSymbol():
	assertExpressionRead("'a ", "'a")

def test_readEmptyParentheses():
	assertExpressionRead("() ", "nil")
