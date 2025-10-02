type Input = lang(r'[^a].*|a(.(ba.)*(b(a|[^a].*)|([^b]b*)*))?')


def f340(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s) + 3)
    inv(isinstance(s[i:], Input))
    assert s[i] == "a"
    assert s[i + 2] == "b"
    i += 3
