type Input = lang(r'[^a].*|a([^ab](ba[^ab])*(b([^a].*|a([ab][^ab]*)*)|([^b]b*)*)|([ab][^ab]*)*)')


def f450(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s) + 3)
    inv(isinstance(s[i:], Input))
    assert s[i + 0] == "a"
    assert s[i + 1] != "a"
    assert s[i + 1] != "b"
    assert s[i + 2] == "b"
    i += 3
