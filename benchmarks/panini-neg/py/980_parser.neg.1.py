type Input = lang(r'([^a]([^b]b*)*|a..*)?')


def parser(s: Input):
  if s[0] == "a":
    assert len(s) == 1
  else:
    assert s[1] == "b"
