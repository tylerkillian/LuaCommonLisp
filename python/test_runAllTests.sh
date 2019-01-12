set -e

for nextTest in $( ls test_*.py | sed 's/.py$//' )
do
	python3 tester.py $nextTest
done

assertEquals() {
	# Wrap each command's output with periods to avoid
	# trimming any whitespace or newlines
	testValue=`echo . ; $1 ; echo .`
	referenceValue=`echo . ; $2 ; echo .`
	if [ "$testValue" != "$referenceValue" ]; then
		exit 1
	fi
}

for nextTest in $( ls test_*.lisp | sed 's/.lisp$//' )
do
	if [ -e "$nextTest.capture" ]
	then
		echo "Testing output for $nextTest.lisp"
		referenceCommand="cat $nextTest.capture"
		testCommand="python3 main.py -q $nextTest.lisp"
		assertEquals "$testCommand" "$referenceCommand"
	else
		echo "Running tests in $nextTest.lisp"
		python3 main.py -q $nextTest.lisp
	fi
done
