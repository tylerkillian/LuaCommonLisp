from character import is_whitespace, is_constituent_character

read = None

def Llist(elements):
    return {
        'form': 'list',
        'elements': elements
    }

def _read_s_expression(x, input_stream):
    elements = []
    next_token = None
    while not input_stream.at_eof():
        x = input_stream.read()
        if x == ')':
            return Llist(elements)
        elif is_whitespace(x):
            continue
        else:
            input_stream.unread(x)
            next_token = read(input_stream)
            if next_token:
                elements.append(next_token)

def Quote(arg):
    return {
        'form': 'quote',
        'arg': arg
    }

def _read_quote(x, input_stream):
    return Quote(read(input_stream))

def Backquote(arg):
    return {
        'form': 'backquote',
        'arg': arg
    }

def _read_backquote(x, input_stream):
    return Backquote(read(input_stream))

def Comma(arg):
    return {
        'form': 'comma',
        'arg': arg
    }

def Splice(arg):
    return {
        'form': 'splice',
        'arg': arg
    }

def _read_comma(x, input_stream):
    if not input_stream.at_eof():
        y = input_stream.read()
        if y == '@':
            return Splice(read(input_stream))
        else:
            input_stream.unread(y)
            return Comma(read(input_stream))

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

