set -e

python3 main.py -q test_atoms.lisp
python3 main.py -q test_basic.lisp

for nextTest in $( ls test_*.py | sed 's/.py$//' )
do
	python3 tester.py $nextTest
done

source test_utilities.sh
for nextTest in $( ls *.capture | sed 's/.capture$//' )
do
	echo "capture $nextTest"

	referenceCommand="cat $nextTest.capture"
	testCommand="python3 main.py -q $nextTest.lisp"
	assertEquals "$testCommand" "$referenceCommand"
done
