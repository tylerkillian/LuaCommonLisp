def _is_whitespace(x):
    if x == ' ':
        return True
    return False

def _is_macro_character(x):
    if x == '(':
        return True
    return False

def _is_constituent_character(x):
    if x >= 'a' and x  <= 'z':
        return True
    if x >= 'A' and x  <= 'Z':
        return True
    return False

def _read_s_expression(x, input_stream):
    print(x)
    result = []
    next_token = None
    while not input_stream.at_eof():
        x = input_stream.get_next_character()
        print(x)
        if x == ')':
            return result
        elif _is_whitespace(x):
            continue
        else:
            input_stream.prepend(x)
            read(input_stream)

def _get_macro_reader(x):
    if x == '(':
        return _read_s_expression

def read(input_stream):
    token = ''
    while not input_stream.at_eof():
        x = input_stream.get_next_character()
        print('read got ' + x)
        if _is_whitespace(x) and token == '':
            continue
        elif _is_macro_character(x):
            macro_reader = _get_macro_reader(x)
            macro_reader_result = macro_reader(x, input_stream)
            if macro_reader_result:
                return macro_reader_result
            continue
        elif _is_constituent_character(x):
            token += x
            print('token is now ' + token)
        else:
            print('elsing')
            return token
    return
