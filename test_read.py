import read
import Stream

def run_tests():
    stream = Stream.create("()")
    result = read.read(None, stream)
    assert result == []

    stream = Stream.create("(a)")
    result = read.read(None, stream)
    assert result == ["a"]

    print("test_read : SUCCESS")
