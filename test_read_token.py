import read_token
import Stream

def run_tests():
    stream = Stream.create("a")
    result = read_token.read_token(stream)
    assert result == "a" 

    print("test_read_token : SUCCESS")

