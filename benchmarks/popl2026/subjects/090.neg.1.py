type Input = lang(r'([^a]|a.).*')

def f090(s: Input):
  if len(s) > 0:
    assert s == "a"
