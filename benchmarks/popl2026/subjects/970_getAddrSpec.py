type Input = lang(r'[^<>]*<[^>]*>.*')

def getAddrSpec(email: Input):
  b = email.index('<',0)+1
  return email[b:email.index('>',b)]
