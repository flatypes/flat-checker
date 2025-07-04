type Input = lang(r'a*([^ab]|bb*[^b]).*')

def f240(s: Input):
  # ghost var
  first_b = s.find("b")
  if first_b < 0:
    first_b = len(s)

  i = 0
  while i < len(s):
    inv(0 <= i <= first_b)
    if s[i] != "a":
      break
    i += 1
  while i < len(s):
    inv(first_b <= i <= len(s))
    if s[i] != "b":
      break
    i += 1
  assert i == len(s)
