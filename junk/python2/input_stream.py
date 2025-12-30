class InputStream:
    def __init__(self, data=''):
        self.data = data
    def at_eof(self):
        if len(self.data) == 0:
            return True
        return False
    def read(self):
        if not self.data:
            return
        result = self.data[0]
        self.data = self.data[1:]
        return result
    def unread(self, x):
        if not self.data:
            self.data = x
        else:
            self.data = x + self.data
    def append(self, data_to_append):
        self.data += data_to_append
