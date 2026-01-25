import read_string
import streams

def run_tests():
    stream = streams.create('""')
    result = read_string.read_string(stream, streams.get_next_character(stream))
    assert result == "" 

    stream = streams.create('"hello"')
    result = read_string.read_string(stream, streams.get_next_character(stream))
    assert result == "hello" 
