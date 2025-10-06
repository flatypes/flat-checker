type Input = lang(r'(...*)?')


def f014(s: Input) -> Input:
  if not len(s) == 1:
    raise Exception
  return s[0:1]
