type Input = lang(r'([^a]|a..*)*')


def f194(s: Input):
  i = len(s) - 1
  assert s[i] == "a"
  while i > 0:
    i = i - 1
    assert s[i] != "a"
