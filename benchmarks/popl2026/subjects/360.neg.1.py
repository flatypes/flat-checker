type Input = lang(r'a(.*[^b])?|([^a]a*)*')

def f360(s: Input):
  assert s[0] == "a"
  assert s[len(s)-1] == "b"
