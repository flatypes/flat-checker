type Input = lang(r'a([^b](b..*|([^b]b*)*)|b([^b]|b.).*)?|([^a]a*)*')

def f350(s: Input):
  if s == "ab":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"
