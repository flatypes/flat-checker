import os
import json
import csv
from typing import Callable


def collect(path: str, metrics: list[str]) -> dict: # metric -> value (int or float)
    results = {}
    with open(path, 'r') as f:
        root: dict = json.load(f)
        data: dict = root['files'][0]
        if 'subgoals' in data:
            results['subgoals'] = len(data['subgoals'])
            aggregation: dict = data['subgoals/aggregation']
            for m in metrics:
                results[m] = aggregation.get(m, 0)
        else:
            results['subgoals'] = 0
            for m in metrics:
                results[m] = 0
    
    return results

def aggregate(benchmark: str, metrics: list[str],
              filter: Callable[[str], str]) -> dict: # metric -> value (int or float)
    results_dir = f'methods/flat-checker/results-{benchmark}'
    total_values = {m: 0 for m in metrics}
    for f in os.listdir(results_dir):
        if f.endswith('.json'):
            subject = f[:-5]
            if filter(subject):
                path = os.path.join(results_dir, f)
                values = collect(path, metrics)
                for m in metrics:
                    total_values[m] += values[m]

    return total_values

CATEGORIES_CSV = "panini/categories.csv"
subject_to_category = {}
with open(CATEGORIES_CSV, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        subject_to_category[row["subject"][:3]] = row["category"]

def process(benchmarks: list[str], metrics: list[str]) -> None:
    categories = set(subject_to_category.values())
    
    os.makedirs('results', exist_ok=True)
    for benchmark in benchmarks:
        with open(f'results/metrics-{benchmark}.csv', 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=['category', *metrics])
            writer.writeheader()

            for c in categories:
                d = aggregate(benchmark, metrics, lambda s: subject_to_category[s[:3]] == c)
                writer.writerow({'category': c, **d})

            d = aggregate(benchmark, metrics, lambda s: True)
            writer.writerow({'category': 'total', **d})

if __name__ == "__main__":
    benchmarks = ['panini', 'panini-neg']
    metrics = ['smt queries', 'smt queries/valid', 'lemmas', 'narrow']
    process(benchmarks, metrics)
