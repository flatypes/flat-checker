type Input = lang(r'ab*[^b].*|([^a]a*)*')


def f233(s: Input):
  i = 0
  assert s.index("a", i) == i
  i += 1
  while i < len(s):
    inv(1 <= i <= len(s))
    assert s.index("b", i) == i
    i += 1
