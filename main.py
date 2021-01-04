from standard_read_definition import read
from evaluate import evaluate
from lprint import lprint

input_stream = InputStream()

for line in sys.stdin:
    input_stream.append(line[:-1])
    form = read(input_stream)
    result = evaluate(form)
    lprint(result)
