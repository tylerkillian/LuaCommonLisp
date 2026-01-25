import glob
import importlib
import test_cl
import test_read
import test_read_s_expression
import test_read_string
import test_read_token

def run_tests():
    for filename in glob.glob("test_*.py"):
        test_module = importlib.import_module(filename[:-3])
        test_module.run_tests()
        print(filename[:-3], ": SUCCESS")

run_tests()
