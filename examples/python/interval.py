from flat.py import range

def f(x: range[1:5], y: range[20:30]) -> range[21:36]:
    z: range[21:35] = x + y
    return z + 1
