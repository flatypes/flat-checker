import os
import csv

def merge(out_path: str):
  merged = {} # (str, int) -> dict[str, str], 
              # (subject, goal) -> { method_1: result, method_2: result, ... }
  methods = []
  # read
  for method in os.listdir('.'):
    csv_path = f'{method}/results_paper/results.csv'
    if os.path.exists(csv_path):
      methods.append(method)
      print(f'Reading {csv_path}')
      with open(csv_path, newline='') as csv_file:
        reader = csv.DictReader(csv_file, delimiter=',')
        for row in reader:
          if row['goal'] is not None and row['goal'].isdigit():
            key = row['subject'], int(row['goal'])
            if key not in merged:
              merged[key] = {}
            merged[key][method] = row['result']
          else:
            print(f'Ignore bad line: {row}')

  # write
  with open(out_path, 'w', newline='') as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=['subject', 'goal'] + methods, delimiter=',')
    writer.writeheader()
    for s, g in merged:
      d = merged[s, g]
      d['subject'] = s
      d['goal'] = g
      writer.writerow(d)

merge('overall_results.csv')