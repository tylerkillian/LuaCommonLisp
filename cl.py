import read
import Stream
import sys

def get_file_contents(filename):
    with open(filename) as f:
        return f.read()

def main(filename):
    contents = get_file_contents(filename)
    stream = Stream.create(contents)
    code = read.read(None, stream)
    while code:
        print("code =", code)
        code = read.read(None, stream)

#main(sys.argv[1])
