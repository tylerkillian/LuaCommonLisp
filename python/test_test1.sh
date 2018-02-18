echo "test_test1"

assertEquals() {
	testValue="$1"
	referenceValue="$2"
	if [ "$testValue" != "$referenceValue" ]
	then
		callerInfo=`caller`
		echo "ASSERT FAILED! [Line $callerInfo]"
		echo ""
		echo "EXPECTED:"
		echo "$referenceValue"
		echo ""
		echo "GOT:"
		echo "$testValue"
		exit 1
	fi
}

referenceOutput=`clisp test1.lisp`
testOutput=`python3 main.py test1.lisp`
assertEquals "$testOutput" "$referenceOutput"

