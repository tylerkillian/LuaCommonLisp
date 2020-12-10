from repl import repl

class FakeReader:
    def __init__(self, *tokens):
        self.tokens = []
        for token in tokens:
            self.tokens.append(token)
    def read(self):
        if len(self.tokens) == 0:
            return
        else:
            return self.tokens.pop(0)

class FakeEvaluator:
    def __init__(self, **expressionToValueMapping):
        self.expressionToValueMapping = {}
        for expression in expressionToValueMapping:
            self.expressionToValueMapping[expression] = expressionToValueMapping[expression]
    def evaluate(self, expression):
        return self.expressionToValueMapping[expression]

class FakePrinter:
    def __init__(self):
        self.output = ""
    def print(self, value):
        self.output = self.output + value
    def getAllOutput(self):
        return self.output

def test_repl():
    reader = FakeReader("expression1", "expression2", "expression3")
    evaluator = FakeEvaluator(expression1 = "value1", expression2 = "value2", expression3 = "value3")
    printer = FakePrinter()

    result = repl(reader, evaluator, printer)

    assert(printer.getAllOutput() == "value1value2value3")
