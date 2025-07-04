
def f500(s: Input):
  i = 0
  # ghost
  first_b = s.find('b') if 'b' in s else s.find('c') if 'c' in s else len(s)
  first_c = s.find('c') if 'c' in s else len(s)
  while i < len(s):
    inv(0 <= i <= first_b)
    if s[i] != "a":
      break
    i += 1
  while i < len(s):
    inv(first_b <= i <= first_c)
    if s[i] != "b":
      break
    i += 1
  while i < len(s):
    inv(first_c <= i <= len(s))
    if s[i] != "c":
      break
    i += 1
  assert i == len(s)
