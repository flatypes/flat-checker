type Input = lang(r'[^a]*(a([^ab]b*|b([^a]|a(([^ab]b*)*a)*b)*([^a]|a(a|[^ab]b*)*)|([^ab]b*)*a)*)?')

def f311(s: Input):
  assert s[len(s)-2:len(s)] == "ab"
