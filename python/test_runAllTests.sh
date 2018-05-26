set -e

python3 main.py -q test_atoms.lisp
python3 main.py -q test_basic.lisp

for nextTest in $( ls test_*.py | sed 's/.py$//' )
do
	python3 tester.py $nextTest
done


for nextTest in $( ls *.capture )
do
	echo $nextTest
done

# remove these
bash test_hello.sh
bash test_test1.sh
bash test_let.sh
bash test_sum.sh


