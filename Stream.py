def create(contents):
    return {
        "contents": contents,
        "offset": 0
    }

def get_next_character(stream):
    if stream["offset"] < len(stream["contents"]):
        result = stream["contents"][stream["offset"]]
        stream["offset"] += 1
        return result
    else:
        return

def at_end_of_file(stream):
    if stream["offset"] >= len(stream["contents"]):
        return True
    else:
        return False
