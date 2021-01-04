from repl import repl

CODE = """
(setf a (+ 1 2 3))
"""

def run_tests():
    repl(CODE)
