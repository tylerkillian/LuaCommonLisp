import Stream
import read_s_expression

def fake_read(environment, stream):
    x = Stream.see_next_character(stream)
    while x in " ":
        Stream.get_next_character(stream)
        x = Stream.see_next_character(stream)

    result = Stream.get_next_character(stream)
    x = Stream.see_next_character(stream)
    while not x in " )":
        result += Stream.get_next_character(stream)
        x = Stream.get_next_character(stream)
    return result

def run_tests():
    stream = Stream.create("()")
    result = read_s_expression.read_s_expression(fake_read, stream, Stream.get_next_character(stream))
    assert result == []

    stream = Stream.create("(a)")
    result = read_s_expression.read_s_expression(fake_read, stream, Stream.get_next_character(stream))
    assert result == ["a"]

    stream = Stream.create("(a b c)")
    result = read_s_expression.read_s_expression(fake_read, stream, Stream.get_next_character(stream))
    assert result == ["a", "b", "c"]

    print("test_read_s_expression : SUCCESS")
