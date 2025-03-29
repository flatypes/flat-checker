type Input = lang(r'.*a.*')

def f142(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(i <= s.find('a'))
    if s[i] == "a":
      return
    i += 1
  raise Exception
