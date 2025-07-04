type Input = lang(r'a(b..*|([^b]b*)*)|c..*|([^ac][ac]*)*')

def f440(s: Input):
  assert s == "ab" or s == "c"
