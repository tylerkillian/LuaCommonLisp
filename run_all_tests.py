import test_cl
import test_read
import test_read_s_expression
import test_read_token

def run_tests():
    test_read_token.run_tests()
    test_read_s_expression.run_tests()
    test_read.run_tests()
    test_cl.run_tests()

run_tests()
