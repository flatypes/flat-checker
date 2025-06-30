type Input1 = lang(r'a+')
type Input2 = lang(r'b+')


def f(s1: Input1, s2: Input2):
  s = s1 + s2
  show_type(s)
  show_type(s[0])
  show_type(s[1])
  show_type(s[2:])
