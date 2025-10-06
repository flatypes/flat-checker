type Input = lang(r'[^a].*|a([^b]b*|b(a|[^a].*))*')


def f250(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s) + 2)
    inv(isinstance(s[i:], Input))
    assert s[i] == "a"
    assert s[i + 1] == "b"
    i += 2
