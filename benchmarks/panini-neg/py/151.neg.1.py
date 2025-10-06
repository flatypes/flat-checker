type Input = lang(r'[^a]..*|(a[^a]*)*')


def f151(s: Input):
  assert len(s) == 1
  assert s != "a"
