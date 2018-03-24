from Stream import *

def test_popSingleCharacter():
	stream = Stream()
	stream.push("abc")
	assert(stream.popCharacter() == "a")
	assert(stream.popCharacter() == "b")
	assert(stream.popCharacter() == "c")
	assert(not stream.popCharacter())

def test_constructor():
	stream = Stream("abc")
	assert(stream.popCharacter() == "a")
	assert(stream.popCharacter() == "b")
	assert(stream.popCharacter() == "c")
	assert(not stream.popCharacter())

def test_peekNextCharacter():
	stream = Stream("ab")
	assert(stream.peekNextCharacter() == "a")
	stream.popCharacter()
	assert(stream.peekNextCharacter() == "b")

