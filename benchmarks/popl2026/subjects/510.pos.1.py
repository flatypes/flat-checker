type Input = lang(r'a?b?c?')

def f510(s: Input):
  assert s == "abc" or s == "ab" or s == "a" or s == "ac" or s == "bc" or s == "b" or s == "c" or s == ""
