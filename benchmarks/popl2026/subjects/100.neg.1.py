type Input = lang(r'a(a..*|([^a]a*)*)|([^a]a*)*')

def f100(s: Input):
  assert s == "aa"
