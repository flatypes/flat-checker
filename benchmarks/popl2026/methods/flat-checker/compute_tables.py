import csv
import math
from collections import defaultdict

results_dir='results'

def read_csv_dict(filepath):
    with open(filepath, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))

def write_csv_dict(filepath, fieldnames, rows):
    with open(filepath, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

def mean(values):
    return sum(values) / len(values) if values else 0.0

def stdev(values):
    n = len(values)
    if n < 2:
        return 0.0
    avg = mean(values)
    variance = sum((x - avg) ** 2 for x in values) / (n - 1)
    return math.sqrt(variance)

# Load data
categories = read_csv_dict("../../subjects/categories.csv")    # subject,category
results = read_csv_dict(results_dir + "/results.csv")                # subject,time_ms,status,success
results_vc = read_csv_dict(results_dir + "/results_vc.csv")          # subject,goal,time_ms,success
invariants = read_csv_dict("invariants.csv")          # subject,inv

# Build lookups
subject_to_category = {row['subject']: row['category'] for row in categories}
subject_to_inv = {row['subject']: int(row['inv']) for row in invariants}

# Grouping data by category
cat_subjects = defaultdict(set)
cat_goals_count = defaultdict(int)
cat_success_values = defaultdict(list)
cat_time_values = defaultdict(list)
cat_inv_counts = defaultdict(int)

# Process results
for row in results:
    subject = row['subject']
    category = subject_to_category.get(subject)
    if not category:
        continue

    cat_subjects[category].add(subject)
    # success column assumed convertible to bool or int
    success = row['success'].lower() in ('true', '1', 'yes')
    cat_success_values[category].append(success)

    try:
        time_ms = float(row['time_ms'])
        cat_time_values[category].append(time_ms)
    except ValueError:
        pass

# Process results_vc for goals count
for row in results_vc:
    subject = row['subject']
    category = subject_to_category.get(subject)
    if not category:
        continue
    cat_goals_count[category] += 1

# Aggregate num_inv per category by summing per-subject invariants
for subject, inv in subject_to_inv.items():
    category = subject_to_category.get(subject)
    if category:
        cat_inv_counts[category] += inv

# Build output rows
rows = []
for category in sorted(cat_subjects.keys()):
    subjects = cat_subjects[category]
    num_subjects = len(subjects)
    num_goals = cat_goals_count.get(category, 0)
    num_inv = cat_inv_counts.get(category, 0)
    num_verified = sum(cat_success_values.get(category, []))
    avg_time_ms = mean(cat_time_values[category]) if cat_time_values[category] else 0.0
    avg_time_stdev = stdev(cat_time_values[category]) if cat_time_values[category] else 0.0

    rows.append({
        'category': category,
        'num_subjects': num_subjects,
        'num_goals': num_goals,
        'num_inv': num_inv,
        'num_verified': num_verified,
        'avg_time_ms': avg_time_ms,
        'avg_time_stdev': avg_time_stdev
    })

# Compute totals
all_subjects = set(row['subject'] for row in categories)
total_num_subjects = len(all_subjects)
total_num_goals = len(results_vc)
total_num_inv = sum(subject_to_inv.values())
total_num_verified = sum(sum(lst) for lst in cat_success_values.values())
total_times = [float(r['time_ms']) for r in results if r['time_ms']]
total_avg_time = mean(total_times) if total_times else 0.0
total_avg_stdev = stdev(total_times) if total_times else 0.0

rows.append({
    'category': 'total',
    'num_subjects': total_num_subjects,
    'num_goals': total_num_goals,
    'num_inv': total_num_inv,
    'num_verified': total_num_verified,
    'avg_time_ms': total_avg_time,
    'avg_time_stdev': total_avg_stdev
})

# Write output
fieldnames = ['category', 'num_subjects', 'num_goals', 'num_inv', 'num_verified', 'avg_time_ms', 'avg_time_stdev']
write_csv_dict(results_dir + "/results_by_category.csv", fieldnames, rows)
