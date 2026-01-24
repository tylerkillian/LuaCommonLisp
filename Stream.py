def create(contents):
    return {
        "contents": contents,
    }

def get_next_character(stream):
    if stream["contents"]:
        result = stream["contents"][0]
        stream["contents"] = stream["contents"][1:]
        return result
    else:
        return

def see_next_character(stream):
    if stream["contents"]:
        return stream["contents"][0]
    else:
        return

def prepend(stream, x):
    stream["contents"] = x + stream["contents"]

def at_end_of_file(stream):
    if not stream["contents"]:
        return True
    else:
        return False
