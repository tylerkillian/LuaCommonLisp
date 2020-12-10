source test_utilities.sh

echo "test_dynamicScope"

referenceCommand="clisp test_dynamicScope.lisp"
testCommand="python3 main.py test_dynamicScope.lisp"
assertEquals "$testCommand" "$referenceCommand"

