import Stream
import sys

def is_whitespace(x):
    return x in " \n"

def is_macro(x):
    return x in "()"

def see_next_character(stream):
    save = ""
    x = Stream.get_next_character(stream)
    save += x
    while is_whitespace(x):
        x = Stream.get_next_character(stream)
        save += x
    result = x
    for x in save[::-1]:
        Stream.prepend(stream, x)
    return result

def read_s_expression(read, stream, x):
    print("read_s_expression got", x)
    print(see_next_character(stream))
    print(see_next_character(stream))
    result = []
    while see_next_character(stream) != ")":
        print("looping")
        result.append(read(None, stream))
    return result

