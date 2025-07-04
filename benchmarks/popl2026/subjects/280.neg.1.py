type Input = lang(r'[^a].*|a(b..*|([^b]b*)*)')

def f280(s: Input):
  assert s == "" or s == "ab"
