type Input = lang(r'[^a](a..*|([^a]a*)*)|(a[^a]*)*')

def f180(s: Input):
  assert s[0] != "a"
  assert s[1] == "a"
  assert len(s) == 2
