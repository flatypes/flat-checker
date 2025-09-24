type Input = lang(r'([^ab]|aa*[^a]|b.).*')


def f381(s: Input):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  if i < len(s):
    assert s == "b"
