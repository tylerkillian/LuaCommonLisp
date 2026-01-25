import read_token
import streams

def run_tests():
    stream = streams.create("a")
    result = read_token.read_token(stream)
    assert result == "a" 

    stream = streams.create("abc")
    result = read_token.read_token(stream)
    assert result == "abc"
