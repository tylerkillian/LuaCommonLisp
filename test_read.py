import read
import streams

def run_tests():
    stream = streams.create("()")
    result = read.read(None, stream)
    assert result == []

    stream = streams.create("(a)")
    result = read.read(None, stream)
    assert result == ["a"]
