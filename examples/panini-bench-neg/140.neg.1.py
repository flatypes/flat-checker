type Input = lang(r'[^a]*')


def f140(s: Input) -> int:
  return s.index("a")

# note: the regex [^a]*a.* is equivalent to .*a.*
