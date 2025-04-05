type Input = lang(r'a*b')

def f223(s: Input):
  i = 0
  while i < len(s)-1:
    # 0 <= i < |s| - 1 => |s| >= 2 => s in aa+b
    # s[i:] in aa+b
    # s[i:].index("a") == 0
    assert s.index("a", i) == i
    i += 1
  assert s.index("b",i) == i
