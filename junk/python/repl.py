def repl(reader, evaluator, printer):
    expression = reader.read()
    while expression:
        value = evaluator.evaluate(expression)

        printer.print(value)

        expression = reader.read()
