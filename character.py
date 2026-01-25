def is_whitespace(x):
    return x in " \n"

def is_macro(x):
    return x in '()"'

def is_terminating_macro(x):
    return x in ")"

def is_constituent(x):
    return not is_whitespace(x) and not is_macro(x)
