type Input = lang(r'(a|[^a].).*')


def f163(s: Input):
  i = 0
  while i < len(s):
    assert s[i] != "a"
    i += 1
  assert i < 2
