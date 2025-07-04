type Input = lang(r'')
type Output = lang(r'|.')

def f053(s: Input) -> Output:
  if len(s) == 1:
    return ""
  else:
    return s[0]
