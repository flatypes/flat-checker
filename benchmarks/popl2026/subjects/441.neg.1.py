type Input = lang(r'a(b..*|([^b]b*)*)|c..*|([^ac][ac]*)*')

def f441(s: Input):
  if s[0] == "a":
    assert s[1] == "b"
    assert len(s) <= 2
  else:
    assert s[0] == "c"
    assert len(s) == 1
