import os

rootPath = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

class Sandbox:
    def __init__ (self, pathFunc):
        self.pathFunc = pathFunc

    def path(self):
        return self.pathFunc(self)

def local(rootPath, libPath):
    def path(sbox):
        return os.path.join(rootPath, 'dist', libPath)
    return Sandbox(path)

def glob(rootPath, libPath):
    def path(sbox):
        return os.path.join(rootPath, 'dist', 'globalSbox')
    return Sandbox(path)

