type Input = lang(r'a(b..*|([^b]b*)*)|([^a]a*)*')


def f211(s: Input):
  assert s[0] == "a"
  assert s[1] == "b"
  assert len(s) == 2
