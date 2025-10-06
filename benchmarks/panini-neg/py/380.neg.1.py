type Input = lang(r'([^ab]|aa*[^a]|b.).*')


def f380(s: Input):
  if s == "b":
    return
  else:
    i = 0
    while i < len(s):
      assert s[i] == "a"
      i += 1
