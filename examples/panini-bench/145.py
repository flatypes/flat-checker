type Input = lang(r'.*a.*')

def f145(s: Input):
  i = 0
  f = False
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(f or 'a' not in s[:i])
    f = f or s[i] == "a"
    i += 1
  assert f
