type Input = lang(r'a([^b](b..*|([^b]b*)*)|b([^b]|b.).*)?|([^a]a*)*')

def f351(s: Input):
  assert s[0] == "a"
  if s[1] == "b" and len(s) == 2:
    return
  else:
    assert s[2] == "b"
    assert len(s) == 3
