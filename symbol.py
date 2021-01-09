is_
def Symbol(token):
    return {
        'form': 'symbol',
        'value': token
    }

def read_symbol(x, input_stream):
    token = x
    while not input_stream.at_eof():
        x = input_stream.read()
        if _is_whitespace(x) or _is_macro_character(x):
            input_stream.unread(x)
            return Symbol(token)
        token += x
    return result
