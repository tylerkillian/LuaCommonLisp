import evaluate
import object_s

def test_evaluate_symbol():
    environment = {"a": 3}
    form = object_s.create("symbol", "a")
    result = evaluate.evaluate(environment, form)
    assert result == 3

def run_tests():
    test_evaluate_symbol()
