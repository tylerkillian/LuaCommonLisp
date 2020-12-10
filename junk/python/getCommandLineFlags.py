import sys

def getCommandLineFlags():
	if sys.argv[1] == "-q":
		return {
			"mode" : "quiet",
			"filename" : sys.argv[2]
		}
	else:
		return {
			"mode" : "normal",
			"filename" : sys.argv[1]
		}

