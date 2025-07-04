import os
import csv
from glob import glob
from collections import defaultdict

base_dir = "methods"
all_result_files = glob(os.path.join(base_dir, "*", "results", "results*.csv"))

results = defaultdict(dict)
method_names = set()

for file_path in all_result_files:
    method_name = file_path.split(os.sep)[1]
    method_names.add(method_name)

    with open(file_path, newline='') as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames or []

        # Check for required columns
        if "subject" not in fieldnames or "goal" not in fieldnames:
            print(f"Skipping file {file_path}: missing 'subject' or 'goal' columns")
            continue

        # Decide which column holds the result
        result_col = "success" if "success" in fieldnames else "result" if "result" in fieldnames else None
        if not result_col:
            print(f"Skipping file {file_path}: no 'result' or 'success' column found")
            continue

        for row in reader:
            # Skip empty or incomplete rows
            if not row.get("subject") or not row.get("goal"):
                continue

            key = (row["subject"], row["goal"])
            results[key][method_name] = row.get(result_col, "")

# Sort methods for consistent column order
sorted_methods = sorted(method_names)

with open("overall_results.csv", "w", newline='') as out_f:
    writer = csv.writer(out_f)
    writer.writerow(["subject", "goal"] + sorted_methods)

    for (subject, goal), method_data in sorted(results.items()):
        row = [subject, goal]
        for method in sorted_methods:
            row.append(method_data.get(method, ""))
        writer.writerow(row)
