import Stream
import sys

def get_file_contents(filename):
    with open(filename) as f:
        return f.read()

def is_whitespace(x):
    return x in " \n"

def is_macro(x):
    return x in "()"

def is_constituent(x):
    return not is_whitespace(x)

def read_token(stream):
    result = ""
    if Stream.at_end_of_file(stream):
        return ""

    y = Stream.get_next_character(stream)
    if is_constituent(y):
        return y + read_token(stream)
    elif is_whitespace(y):
        return ""
    return result

def read(environment, stream):
    x = Stream.get_next_character(stream)
    while x:
        if is_whitespace(x):
            continue
        elif is_macro(x):
            reader_macro_function = get_reader_macro_function(x)
            reader_macro_result = reader_macro_function(stream, x)
            if reader_macro_result:
                return reader_macro_result
            else:
                continue
        elif is_constituent(x):
            token = x + read_token(stream)
            return token
        x = Stream.get_next_character(stream)

def main(filename):
    contents = get_file_contents(filename)
    stream = Stream.create(contents)
    code = read(None, stream)
    while code:
        print("code =", code)
        code = read(None, stream)

main(sys.argv[1])
