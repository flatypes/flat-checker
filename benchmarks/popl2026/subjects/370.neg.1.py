type Input = lang(r'[ab]..*|([^ab][ab]*)*')

def f370(s: Input):
  assert s == "a" or s == "b"
