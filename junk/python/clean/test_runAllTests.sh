for nextTest in $( ls test_*.py | sed 's/.py$//' )
do
	python3 tester.py $nextTest
done

echo "ALL TESTS SUCCESSFUL"

