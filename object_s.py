def create(type_, value):
    return {
        "type": type_,
        "value": value
    }

def get_type(object_):
    return object_["type"]

def get_value(object_):
    return object_["value"]
