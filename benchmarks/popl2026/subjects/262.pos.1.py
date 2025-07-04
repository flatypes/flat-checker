type Input = lang(r'a?b')

def f262(s: Input):
  bi = s.index("b")
  if bi == 1:
    assert s[0] == "a"
    assert len(s) == 2
  else:
    assert bi == 0
    assert len(s) == 1
