type Input = lang(r'(a|[^ab][ab]*|b..*)*')


def f221(s: Input):
  i = 0
  while i < len(s):
    inv(0 <= i < len(s))
    if s[i] != "a":
      break
    i += 1
  assert s[i] == "b"
