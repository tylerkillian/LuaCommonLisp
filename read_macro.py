from character import is_whitespace, is_constituent_character

read = None

def _read_s_expression(x, input_stream):
    result = {
        'form': 'list',
        'elements': []
    }
    next_token = None
    while not input_stream.at_eof():
        x = input_stream.read()
        if x == ')':
            return result
        elif is_whitespace(x):
            continue
        else:
            input_stream.unread(x)
            next_token = read(input_stream)
            if next_token:
                result['elements'].append(next_token)

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

def _read_comma(x, input_stream):
    form = 'comma'
    if not input_stream.at_eof():
        y = input_stream.read()
        if y == '@':
            form = 'splice'
        else:
            input_stream.unread(y)
    return {
        'form': form,
        'arg': read(input_stream)
    }

def get_macro_reader(x):
    if x == '(':
        return _read_s_expression
    elif x == "'":
        return _read_quote
    elif x == '`':
        return _read_backquote
    elif x == ',':
        return _read_comma

def is_macro_character(x):
    if x in "()'`,":
        return True
    return False

