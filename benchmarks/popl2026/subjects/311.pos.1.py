type Input = lang(r'.*ab')

def f311(s: Input):
  assert s[len(s)-2:len(s)] == "ab"
