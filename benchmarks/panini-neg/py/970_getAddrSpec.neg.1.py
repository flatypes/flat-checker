type Input = lang(r'[^<>]*((<[^<>]*)*|(>[^>]*)*)')
type Output = lang(r'[^>]*')


def getAddrSpec(email: Input) -> Output:
  b = email.index('<', 0) + 1
  return email[b:email.index('>', b)]
