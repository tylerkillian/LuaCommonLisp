def _is_whitespace(c):
    if c == ' ':
        return True
    return False

def _is_constituent_character(c):
    if c > 'a' and c  < 'z':
        return True
    if c > 'A' and c  < 'Z':
        return True
    return False

def read(input_stream):
    token = ''
    while not input_stream.at_eof():
        next_character = input_stream.get_next_character()
        if _is_whitespace(next_character) and token == '':
            continue
        elif _is_constituent_character(next_character):
            token += next_character
        else:
            return token
    return
