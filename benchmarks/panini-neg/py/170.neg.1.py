type Input = lang(r'[^a]*a.*')


def f170(s: Input):
  assert s.find("a") == -1
