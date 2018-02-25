source test_utilities.sh

echo "test_sum"

referenceCommand="clisp test_sum.lisp'
testCommand="python3 main.py test_sum.lisp"
assertEquals "$testCommand" "$referenceCommand"

