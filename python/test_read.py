from read import *
from Stream import Stream
from Node import isSymbol

def test_readSymbol():
	inputStream = Stream("a ")
	symbol = read(inputStream)
	assert(isSymbol(symbol, "a"))
