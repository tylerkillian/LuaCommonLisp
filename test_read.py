import read
import streams

def test_read_token():
    stream = streams.create("a")
    result = read.read_token(stream)
    assert result == "a"

    stream = streams.create("abc")
    result = read.read_token(stream)
    assert result == "abc"

def test_read_string():
    stream = streams.create('""')
    result = read.read_string(stream, streams.get_next_character(stream))
    assert result == "" 

    stream = streams.create('"hello"')
    result = read.read_string(stream, streams.get_next_character(stream))
    assert result == "hello" 

def fake_read(environment, stream):
    x = streams.see_next_character(stream)
    while x in " ":
        streams.get_next_character(stream)
        x = streams.see_next_character(stream)

    result = streams.get_next_character(stream)
    x = streams.see_next_character(stream)
    while not x in " )":
        result += streams.get_next_character(stream)
        x = streams.get_next_character(stream)
    return result

def test_read_s_expression():
    stream = streams.create("()")
    result = read.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == []

    stream = streams.create("(a)")
    result = read.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == ["a"]

    stream = streams.create("(a b c)")
    result = read.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == ["a", "b", "c"]

def run_tests():
    test_read_token()
    test_read_string()
    test_read_s_expression()

    stream = streams.create("()")
    result = read.read(None, stream)
    assert result == []

    stream = streams.create("(a)")
    result = read.read(None, stream)
    assert result == ["a"]
