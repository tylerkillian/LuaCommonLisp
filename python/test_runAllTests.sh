set -e

for nextTest in $( ls test_*.py | sed 's/.py$//' )
do
	python3 tester.py $nextTest
done

#source test_utilities.sh
#for nextTest in $( ls test_*.lisp | sed 's/.lisp$//' )
#do
#	if [ -e "$nextTest.capture" ]
#	then
#		echo "Testing output for $nextTest.lisp"
#		referenceCommand="cat $nextTest.capture"
#		testCommand="python3 main.py -q $nextTest.lisp"
#		assertEquals "$testCommand" "$referenceCommand"
#	else
#		echo "Running tests in $nextTest.lisp"
#		python3 main.py -q $nextTest.lisp
#	fi
#done
