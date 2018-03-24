set -e

python3 tester.py test_reader
python3 tester.py test_Stream
python3 tester.py test_read
bash test_hello.sh
bash test_test1.sh
bash test_let.sh
bash test_sum.sh

