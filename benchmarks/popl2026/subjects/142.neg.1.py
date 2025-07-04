type Input = lang(r'[^a]*')

def f142(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv('a' not in s[:i])
    if s[i] == "a":
      return
    i += 1
  raise Exception
