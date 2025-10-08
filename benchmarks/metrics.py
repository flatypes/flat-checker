import os
import json
import csv
from typing import Callable, Optional
import statistics

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
              filter: Callable[[str], bool]) -> dict: # metric -> value (int or float)
    results_dir = f'methods/flat-checker/results-{benchmark}'
    total_values = {m: 0 for m in metrics}
    count = 0
    for f in os.listdir(results_dir):
        if f.endswith('.json'):
            subject = f[:-5]
            if filter(subject):
                count += 1
                path = os.path.join(results_dir, f)
                values = collect(path, metrics)
                for m in metrics:
                    total_values[m] += values[m]
    
    total_values['count'] = count
    for m in metrics:
        total_values[f'{m}_avg'] = total_values[m] / count

    return total_values

def collect_times(path: str) -> dict: # (subject, goal) -> time (int or None)
    with open(path, 'r') as file:
        reader = csv.DictReader(file)
        return {(row['subject'], row['goal']): get_time(row) for row in reader}

def get_time(row: dict) -> Optional[int]:
    if row['result'] == "timeout":
        return None
    return int(row['time_ms'])

def aggregate_time(benchmark: str, filter: Callable[[str], bool]) -> dict: # metric -> time (int)
    times = collect_times(f'methods/flat-checker/results-{benchmark}/results-{benchmark}.csv')
    ts = [t for (k, _), t in times.items() if filter(k)]
    assert all(type(t) is int for t in ts)
    return {'time_ms_mean': statistics.mean(ts), 'time_ms_max': max(ts), 'time_ms_min': min(ts),
            'time_ms_median': statistics.median(ts), 'time_ms_2nd_max': max([t for t in ts if t != max(ts)])}

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
            metric_fields = ['count'] + [f for m in metrics for f in (m, f'{m}_avg')]
            time_fields = ['time_ms_mean', 'time_ms_max', 'time_ms_min', 'time_ms_median', 'time_ms_2nd_max']
            writer = csv.DictWriter(f, fieldnames=['category', *metric_fields, *time_fields])
            writer.writeheader()

            for c in categories:
                d1 = aggregate(benchmark, metrics, lambda s: subject_to_category[s[:3]] == c)
                d2 = aggregate_time(benchmark, lambda s: subject_to_category[s[:3]] == c)
                writer.writerow({'category': c, **d1, **d2})

            d1 = aggregate(benchmark, metrics, lambda s: True)
            d2 = aggregate_time(benchmark, lambda s: True)
            writer.writerow({'category': 'total', **d1, **d2})

if __name__ == "__main__":
    benchmarks = ['panini', 'panini-neg']
    metrics = ['smt queries/valid', 'smt queries', 'lemmas', 'narrow/nontrivial']
    process(benchmarks, metrics)
