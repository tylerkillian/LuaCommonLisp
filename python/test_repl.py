from repl import repl

def test_repl():
    reader = FakeReader("expression1", "expression2", "expression3")
    evaluator = FakeEvaluator(expression1 = "value1", expression2 = "value2", expression3 = "value3")
    printer = FakePrinter()

    result = repl(reader, evaluator, printer)

    assert(printer.getAllOutput() == "value1\nvalue2\nvalue3\n")
