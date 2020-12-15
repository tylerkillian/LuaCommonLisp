def _is_whitespace(x):
    if x == ' ':
        return True
    return False

def _is_macro_character(x):
    if x == '(':
        return True
    return False

def _get_macro_reader(x):
    if x == '(':
        return read_s_expression

def _is_constituent_character(x):
    if x > 'a' and x  < 'z':
        return True
    if x > 'A' and x  < 'Z':
        return True
    return False

