import os
import subprocess

def all_paths(directory):
  for dirpath, _, filenames in os.walk(directory):
    for f in filenames:
      yield os.path.abspath(os.path.join(dirpath, f))

for path in sorted(all_paths('../smt')):
  print(path)
  out = subprocess.check_output(f'cvc5 --rlimit-per=5000 {path}', shell=True, text=True)
  assert out in ['unsat\n', 'unknown\n'], out
  if out == 'unknown\n':
    print('unknown')
