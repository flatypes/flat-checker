import os
import csv
from typing import Optional


def collect_validity(path: str) -> dict: # (subject, goal) -> result
    with open(path, 'r') as file:
        reader = csv.DictReader(file)
        return {(row['subject'], row['goal']): row['result'] for row in reader}
    
def collect_times(path: str) -> dict: # (subject, goal) -> time (int or None)
    with open(path, 'r') as file:
        reader = csv.DictReader(file)
        return {(row['subject'], row['goal']): get_time(row) for row in reader}
    
def get_time(row: dict) -> Optional[int]:
    if row['result'] == "timeout":
        return None
    return int(row['time_ms'])

def merge_validity(benchmark: str, methods: list[str]) -> dict: # (subject, goal) -> method -> result
    results = {}
    for d in os.listdir('methods'):
        if d in methods:
            path = os.path.join('methods', d, f'results-{benchmark}', f'results-{benchmark}.csv')
            for key, r in collect_validity(path).items():
                if key not in results:
                    results[key] = {}
                results[key][d] = r
    
    return results

def merge_times(benchmark: str, methods: list[str]) -> dict: # (subject, goal) -> method -> time (int or None)
    times = {}
    for d in os.listdir('methods'):
        if d in methods:
            path = os.path.join('methods', d, f'results-{benchmark}', f'results-{benchmark}.csv')
            for key, t in collect_times(path).items():
                if key not in times:
                    times[key] = {}
                times[key][d] = t
    
    return times

def process(benchmarks: list[str], methods: list[str]) -> None:
    os.makedirs('results', exist_ok=True)
    for benchmark in benchmarks:
        results = merge_validity(benchmark, methods)
        with open(f'results/smt-comparison-validity-{benchmark}.csv', 'w', newline='') as f:
            fieldnames = ['subject', 'goal'] + [f'result_{m}' for m in methods]
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for (subject, goal), rs in results.items():
                row = {'subject': subject, 'goal': goal}
                for m in methods:
                    row[f'result_{m}'] = rs.get(m, 'N/A')
                writer.writerow(row)

        times = merge_times(benchmark, methods)
        with open(f'results/smt-comparison-time-{benchmark}.csv', 'w', newline='') as f:
            fieldnames = ['subject', 'goal'] + [f'time_{m}' for m in methods]
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for (subject, goal), ts in times.items():
                row = {'subject': subject, 'goal': goal}
                for m in methods:
                    row[f'time_{m}'] = ts.get(m, 'N/A')
                writer.writerow(row)

        with open(f'results/smt-comparison-summary-{benchmark}.csv', 'w', newline='') as f:
            fieldnames = ['metric'] + methods
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()

            valid_counts = {m: 0 for m in methods}
            invalid_counts = {m: 0 for m in methods}
            timeout_counts = {m: 0 for m in methods}
            error_counts = {m: 0 for m in methods}
            for _, rs in results.items():
                for m in methods:
                    match rs[m]:
                        case 'valid':
                            valid_counts[m] += 1
                        case 'invalid':
                            invalid_counts[m] += 1
                        case 'timeout':
                            timeout_counts[m] += 1
                        case 'error':
                            error_counts[m] += 1
                        case _:
                            assert False, f"Unexpected result {rs[m]} for method {m}"

            writer.writerow({'metric': 'valid', **valid_counts})
            writer.writerow({'metric': 'invalid', **invalid_counts})
            writer.writerow({'metric': 'timeout', **timeout_counts})
            writer.writerow({'metric': 'error', **error_counts})

            total_times = {m: 0 for m in methods}
            for _, ts in times.items():
                for m in methods:
                    if ts[m] is not None:
                        total_times[m] += ts[m]
            for m in methods:
                total_times[m] += timeout_counts[m] * 60000  # timeout is 60s

            writer.writerow({'metric': 'total_time_ms', **total_times})


if __name__ == "__main__":
    benchmarks = ['panini', 'panini-neg']
    methods = ['flat-checker', 'ostrich2', 'z3-noodler-pos', 'cvc5', 'z3']
    process(benchmarks, methods)