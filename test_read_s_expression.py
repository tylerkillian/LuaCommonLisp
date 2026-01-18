import Stream
import read_s_expression

def fake_read(environment, stream):
    return

def run_tests():
    stream = Stream.create("()")
    result = read_s_expression.read_s_expression(fake_read, stream, Stream.get_next_character(stream))
    assert result == []

    print("test_read_s_expression : SUCCESS")
