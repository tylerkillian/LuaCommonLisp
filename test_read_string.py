import read_string
import Stream

def run_tests():
    stream = Stream.create('""')
    result = read_string.read_string(stream, Stream.get_next_character(stream))
    assert result == "" 

    stream = Stream.create('"hello"')
    result = read_string.read_string(stream, Stream.get_next_character(stream))
    assert result == "hello" 

    print("test_read_string : SUCCESS")

