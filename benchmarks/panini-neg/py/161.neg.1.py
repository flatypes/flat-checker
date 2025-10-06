type Input = lang(r'(a|[^a].).*')


def f161(s: Input):
  assert (s == "" or s != "a") and len(s) <= 1
