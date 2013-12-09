import os

def write_if_changed(path, s):
    if os.path.exists(path):
        with open(path, 'r') as file:
            if file.read() == s:
                return
    with open(path, 'w') as file:
        file.write(s)