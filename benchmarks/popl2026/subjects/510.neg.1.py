type Input = lang(r'([^a-c]|a([^bc]|b([^c]|c.)|c.)|b([^c]|c.)|c.).*')

def f510(s: Input):
  assert s == "abc" or s == "ab" or s == "a" or s == "ac" or s == "bc" or s == "b" or s == "c" or s == ""
