from read_common import _is_whitespace, _is_macro_character, _get_macro_reader, _is_constituent_character
from read_s_expression import read_s_expression

def read(input_stream):
    token = ''
    while not input_stream.at_eof():
        x = input_stream.get_x()
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
        else:
            return token
    return
