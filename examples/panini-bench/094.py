type Input = lang(r'a?')

def f094(s: Input):
  if len(s) > 0:    
    t = s[0:1]
    assert t == "a"
    assert len(s) == 1
    return t
  else:
    return s
