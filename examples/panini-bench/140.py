type Input = lang(r'[^a]*a.*')

def f140(s: Input):
  return s.index("a")

# note: the regex [^a]*a.* is equivalent to .*a.*
