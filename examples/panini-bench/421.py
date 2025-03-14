type Input = lang(r'[a-c]')

def f421(s: Input):
  assert len(s) == 1
  if s[0] == "a":
    return
  if s[0] == "b":
    return
  if s[0] == "c":
    return
  raise Exception
