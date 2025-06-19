import os


def all_paths(directory):
  for dirpath, _, filenames in os.walk(directory):
    for f in filenames:
      yield os.path.abspath(os.path.join(dirpath, f))


for path in sorted(all_paths('../smt')):
  print(path)
  os.system(f'cvc5 --rlimit-per=3000 {path}')
  os.system(f'z3 -T:timeout=3 {path}')  # Timeout doesn't work
