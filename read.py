from character import is_whitespace, is_constituent_character

is_macro_character = None
get_macro_reader = None

def read(input_stream):
    token = ''
    while True:
        if input_stream.at_eof():
            if token:
                return token
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
            return token
        elif is_constituent_character(x):
            token += x
        else:
            return token
