from Stream import *

def test_readSingleCharacter():
	stream = Stream()
	stream.write("abc")
	assert(stream.readCharacter() == "a")
	assert(stream.readCharacter() == "b")
	assert(stream.readCharacter() == "c")
	assert(not stream.readCharacter())

def test_constructor():
	stream = Stream("abc")
	assert(stream.readCharacter() == "a")
	assert(stream.readCharacter() == "b")
	assert(stream.readCharacter() == "c")
	assert(not stream.readCharacter())

def test_peekNextCharacter():
	stream = Stream("ab")
	assert(stream.peekNextCharacter() == "a")
	stream.readCharacter()
	assert(stream.peekNextCharacter() == "b")

