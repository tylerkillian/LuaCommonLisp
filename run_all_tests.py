import glob
import importlib

def run_tests():
    for filename in glob.glob("test_*.py"):
        test_module = importlib.import_module(filename[:-3])
        test_module.run_tests()
        print(filename[:-3], ": SUCCESS")

run_tests()
