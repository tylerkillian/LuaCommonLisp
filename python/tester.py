import sys
import importlib

testModule = importlib.import_module(sys.argv[1])
for item in dir(testModule):
	if item[0:5] == "test_":
		print(item)
		getattr(testModule, item)()

