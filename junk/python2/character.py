def is_whitespace(x):
    if x == ' ':
        return True
    return False

def is_constituent_character(x):
    if x in '+-':
        return True
    if x in '0123456789':
        return True
    if x >= 'a' and x  <= 'z':
        return True
    if x >= 'A' and x  <= 'Z':
        return True
    return False
