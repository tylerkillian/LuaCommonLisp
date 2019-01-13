from evaluate import evaluate, createStandardEnvironment

class Evaluator:
	def __init__(self):
		self.environment = createStandardEnvironment()
	def evaluate(self, expression):
		return evaluate(self.environment, expression)

