type Input = lang(r'[^a].*|a(.(b..*|([^b]b*)*))?')

def f331(s: Input):
  if s == "acb" or s == "":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"
