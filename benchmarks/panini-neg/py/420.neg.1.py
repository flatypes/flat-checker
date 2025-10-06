type Input = lang(r'[a-c]..*|([^a-c][a-c]*)*')


def f420(s: Input):
  assert s == "a" or s == "b" or s == "c"
