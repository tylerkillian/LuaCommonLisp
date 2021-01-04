from evaluate import evaluate

ENVIRONMENT = {
    'a': 3
}

def test_variable_evaluation():
    assert evaluate(ENVIRONMENT, 'a') == 3

def run_tests():
    test_variable_evaluation()
