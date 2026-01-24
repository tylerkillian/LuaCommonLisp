import Stream
from character import is_whitespace, is_macro, is_terminating_macro, is_constituent

def read_token(stream):
    result = ""
    if Stream.at_end_of_file(stream):
        return ""

    y = Stream.get_next_character(stream)
    if is_constituent(y):
        return y + read_token(stream)
    elif is_whitespace(y):
        return ""
    elif is_terminating_macro(y):
        Stream.prepend(stream, y)
        return ""
    else:
        assert False

