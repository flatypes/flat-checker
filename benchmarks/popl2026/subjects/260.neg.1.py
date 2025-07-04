type Input = lang(r'a(b..*|([^b]b*)*)|b..*|([^ab][ab]*)*')

def f260(s: Input):
  assert s == "b" or s == "ab"
