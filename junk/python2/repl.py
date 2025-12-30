from input_stream import InputStream
from standard_read_definition import read
from evaluate import evaluate

def repl(code):
    input_stream = InputStream(code)
    environment = {'a': 3}
    while not input_stream.at_eof():
        form = read(input_stream)
        result = evaluate(environment, form)
