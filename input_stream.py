class InputStream:
    def __init__(self, data):
        self.data = data
    def at_eof(self):
        if len(self.data) == 0:
            return True
        return False
    def get_next_character(self):
        if not self.data:
            return
        result = self.data[0]
        self.data = self.data[1:]
        return result
    def prepend(self, x):
        if not self.data:
            self.data = x
        else:
            self.data = x + self.data
