type Input = lang(r'[ab]*[^ab].*')


def f410(s: Input):
  i = 0
  while i < len(s):
    assert s[i] == "a" or s[i] == "b"
    i += 1
