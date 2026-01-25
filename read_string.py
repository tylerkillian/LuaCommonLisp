import streams

def read_string(stream, x):
    assert x == '"'
    result = ""
    while True:
        next_character = streams.get_next_character(stream)
        if next_character == '"':
            break
        result += next_character
    return result
