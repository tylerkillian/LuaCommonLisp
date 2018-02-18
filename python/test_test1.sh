echo "test_test1"

assertEquals() {
	if [ "$1" != "$2" ]
	then
		callerInfo=`caller`
		echo "ASSERT FAILED! [Line $callerInfo]"
		exit 1
	fi
}

referenceOutput=`clisp test1.lisp`
testOutput=`python3 main.py test1.lisp`
assertEquals "$testOutput" "$referenceOutput"

