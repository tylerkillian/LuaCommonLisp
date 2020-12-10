def runTests(tests):
	for testName, testFunction in tests.items():
		print(testName)
		testFunction() 
