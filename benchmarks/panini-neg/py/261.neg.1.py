type Input = lang(r'a(b..*|([^b]b*)*)|b..*|([^ab][ab]*)*')


def f261(s: Input):
  if len(s) == 1:
    assert s[0] == "b"
  elif len(s) == 2:
    assert s[0] == "a"
    assert s[1] == "b"
  else:
    raise Exception
