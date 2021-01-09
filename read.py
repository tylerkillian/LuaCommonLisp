from character import is_whitespace, is_constituent_character
from symbol import Symbol

is_macro_character = None
get_macro_reader = None
read_symbol = None

def read(input_stream):
    token = ''
    while True:
        if input_stream.at_eof():
            if token:
                return Symbol(token)
            return
        x = input_stream.read()
        if is_whitespace(x) and token == '':
            continue
        elif is_macro_character(x) and token == '':
            macro_reader = get_macro_reader(x)
            macro_reader_result = macro_reader(x, input_stream)
            if macro_reader_result:
                return macro_reader_result
            continue
        elif is_macro_character(x) and token != '':
            input_stream.unread(x)
            return Symbol(token)
        elif is_constituent_character(x):
            return read_symbol(x,  input_stream)
        else:
            return Symbol(token)
