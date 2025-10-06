type Input = lang(r'[^a].*|a(b..*|([^b]b*)*)')


def f281(s: Input):
  if len(s) > 0:
    assert s == "ab"
