import sys
import importlib

testModuleName = sys.argv[1]

testModuleAttributes = importlib.import_module(testModuleName)
for attribute in dir(testModuleAttributes):
	if attribute[0:5] == "test_":
		testName = attribute[5:]
		print(testModuleName + " : " + testName)

		testFunction = getattr(testModuleAttributes, attribute)
		testFunction()

