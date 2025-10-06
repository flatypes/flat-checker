type Input = lang(r'[^b]..*|(b[^b]*)*')


def f431(s: Input):
  assert len(s) == 1
  c = s[0]
  if c != "a":
    if c != "c":
      assert c != "b"
