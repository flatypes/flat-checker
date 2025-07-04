type Input = lang(r'a(.(b..*|([^b]b*)*))?|([^a]a*)*')

def f321(s: Input):
  if s == "acb":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"
