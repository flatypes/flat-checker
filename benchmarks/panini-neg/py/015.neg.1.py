type Input = lang(r'(...*)?')


def f015(s: Input) -> Input:
  if not len(s) == 1:
    raise Exception
  return s[0]
