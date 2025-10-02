type Input = lang(r'([^ab]|a([^b]|b.)|b.).*')


def f290(s: Input):
  assert s == "" or s == "a" or s == "b" or s == "ab"
