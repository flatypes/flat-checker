from flat.py import lang

type VersionNumber = lang(r'[0-9]+\.[0-9]+\.[0-9]+')
type Number = lang(r'[0-9]+')


def f(s: VersionNumber) -> Number:
    i1 = s.find('.') + 1
    i2 = s.find('.', i1)
    return s[i1:i2]
