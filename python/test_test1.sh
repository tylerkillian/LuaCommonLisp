echo "test_test1"

referenceOutput=`clisp test1.lisp`
testOutput=`python3 main.py test1.lisp`
if [ "$testOutput" != "$referenceOutput" ]
then
	echo "referenceOutput:"
	echo "$referenceOutput"

	echo "testOutput:"
	echo "$testOutput"

	exit 1
fi

