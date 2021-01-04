def is_symbol(form):
    if type(form) == str:
        return True
    else:
        return False

def is_symbol_macro(form):
    return False

def get_variable_value(environment, name):
    return environment[name]

def evaluate_symbol(environment, form):
    if is_symbol_macro(form):
        # not yet implemented
        assert False
    return get_variable_value(environment, form)

def evaluate(environment, form):
    if is_symbol(form):
        return evaluate_symbol(environment, form)

