def _is_whitespace(x):
    if x == ' ':
        return True
    return False

def _is_macro_character(x):
    if x in "()'`":
        return True
    return False

def _is_constituent_character(x):
    if x in '+-':
        return True
    if x >= 'a' and x  <= 'z':
        return True
    if x >= 'A' and x  <= 'Z':
        return True
    return False

def _read_s_expression(x, input_stream):
    result = []
    next_token = None
    while not input_stream.at_eof():
        x = input_stream.read()
        if x == ')':
            return result
        elif _is_whitespace(x):
            continue
        else:
            input_stream.unread(x)
            next_token = read(input_stream)
            if next_token:
                result.append(next_token)

def _read_quote(x, input_stream):
    return {
        'form': 'quote',
        'arg': read(input_stream)
    }

def _read_backquote(x, input_stream):
    return {
        'form': 'backquote',
        'arg': read(input_stream)
    }

def _get_macro_reader(x):
    if x == '(':
        return _read_s_expression
    elif x == "'":
        return _read_quote
    elif x == '`':
        return _read_backquote

def read(input_stream):
    token = ''
    while True:
        if input_stream.at_eof():
            if token:
                return token
            return

        x = input_stream.read()
        if _is_whitespace(x) and token == '':
            continue
        elif _is_macro_character(x) and token == '':
            macro_reader = _get_macro_reader(x)
            macro_reader_result = macro_reader(x, input_stream)
            if macro_reader_result:
                return macro_reader_result
            continue
        elif _is_macro_character(x) and token != '':
            input_stream.unread(x)
            return token
        elif _is_constituent_character(x):
            token += x
        else:
            return token
