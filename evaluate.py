import object_s

def get_variable_value(environment, variable):
    return environment[variable]

def evaluate_symbol(environment, symbol):
    return get_variable_value(environment, object_s.get_value(symbol))

def evaluate(environment, form):
    form_type = object_s.get_type(form)
    if form_type == "symbol":
        return evaluate_symbol(environment, form)
    elif form_type == "cons":
        return evaluate_cons(form)
    else:
        return evaluate_self_evaluating_object(form)
