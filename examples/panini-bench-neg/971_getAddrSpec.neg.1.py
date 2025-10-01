type Input = lang(r'[^<>]*((<[^<>]*)*|(>[^>]*)*)')
type Output = lang(r'[^>]*')


def getAddrSpec(email: Input) -> Output:
  b1 = email.index('<', 0) + 1
  b2 = email.index('>', b1)
  return email[b1:b2]
