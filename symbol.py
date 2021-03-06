from character import is_whitespace

is_macro_character = None

def Symbol(token):
    return {
        'form': 'symbol',
        'value': token
    }

def read_symbol(x, input_stream):
    token = x
    while not input_stream.at_eof():
        x = input_stream.read()
        if is_whitespace(x) or is_macro_character(x):
            input_stream.unread(x)
            return Symbol(token)
        token += x
    return Symbol(token)
