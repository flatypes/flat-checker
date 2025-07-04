type Input = lang(r'([^ab]|[ab].).*')

def f390(s: Input):
  assert s == "a" or s == "b" or s == ""
