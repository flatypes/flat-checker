type Input = lang(r'([^ab]|aa*[^a]|bb*[^b]).*')

def f400(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(i == 0 if 'a' not in s else True)
    if s[i] != "a":
      break
    i += 1
  if i == 0:
    while i < len(s):
      inv(0 <= i <= len(s))
      if s[i] != "b":
        break
      i += 1
  assert i == len(s)
