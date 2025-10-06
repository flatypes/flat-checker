type Input = lang(r'([^0]|0(1?0)*([^01]|1[^0])).*')


def f470(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i <= len(s))
    inv(isinstance(s[i:], Input))
    assert s[i] == "0"
    if i + 1 < len(s) and s[i + 1] == "1":
      i += 2
    else:
      i += 1
