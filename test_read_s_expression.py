import streams
import read_s_expression

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

def run_tests():
    stream = streams.create("()")
    result = read_s_expression.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == []

    stream = streams.create("(a)")
    result = read_s_expression.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == ["a"]

    stream = streams.create("(a b c)")
    result = read_s_expression.read_s_expression(fake_read, stream, streams.get_next_character(stream))
    assert result == ["a", "b", "c"]
