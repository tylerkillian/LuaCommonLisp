import streams
from character import \
    is_constituent, \
    is_macro, \
    is_terminating_macro, \
    is_whitespace

def read_token(stream):
    result = ""
    while True:
        if streams.at_end_of_file(stream):
            return result

        y = streams.get_next_character(stream)
        if is_constituent(y):
            result += y
        elif is_whitespace(y):
            return result
        elif is_terminating_macro(y):
            streams.prepend(stream, y)
            return result
        else:
            assert False
