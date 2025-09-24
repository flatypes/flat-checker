type Input = lang(r'([^ab]|[ab].).*')


def f391(s: Input):
  if len(s) == 0:
    return
  else:
    if s[0] == "a":
      assert len(s) == 1
    else:
      assert s[0] == "b"
      assert len(s) == 1
