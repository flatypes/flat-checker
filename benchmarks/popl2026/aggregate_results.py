import os
import csv
import math

results_dir="results"

def read_csv(filepath):
    with open(filepath, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))

def safe_float(val):
    try:
        return float(val)
    except:
        return None

def mean(values):
    return sum(values) / len(values) if values else 0.0

def stdev(values):
    n = len(values)
    if n < 2:
        return 0.0
    avg = mean(values)
    return math.sqrt(sum((x - avg) ** 2 for x in values) / (n - 1))

def process_standard_file(method, path):
    rows = read_csv(path)

    subjects = set()
    subjects_sat = set()
    subjects_unsat = set()
    subjects_unknown = set()
    num_goals = len(rows)
    num_sat = 0
    num_unsat = 0
    num_timeouts = 0
    time_all = []
    time_non_timeouts = []

    for row in rows:
        subject = row["subject"]
        subjects.add(subject)
        result = row.get("result", "").lower()
        time_ms = safe_float(row.get("time_ms"))

        if result == "sat":
            num_sat += 1
            subjects_sat.add(subject)
        elif result == "unsat":
            num_unsat += 1
            subjects_unsat.add(subject)
        elif result == "timeout":
            num_timeouts += 1
            subjects_unknown.add(subject)
        else:
            subjects_unknown.add(subject)

        if time_ms is not None:
            time_all.append(time_ms)
            if result != "timeout":
                time_non_timeouts.append(time_ms)

    num_subjects = len(subjects)
    num_subjects_verified = 0
    num_subjects_refuted = 0
    num_subjects_unknown = 0
    for subject in subjects:
        if subject in subjects_unknown:
            num_subjects_unknown += 1
        elif subject in subjects_sat:
            num_subjects_refuted += 1
        elif subject in subjects_unsat:
            num_subjects_verified += 1

    return {
        "method": method,
        "num_subjects": num_subjects,
        "num_subjects_verified": num_subjects_verified,
        "num_subjects_refuted": num_subjects_refuted,
        "num_subjects_unknown": num_subjects_unknown,
        "num_goals": num_goals,
        "num_sat": num_sat,
        "num_unsat": num_unsat,
        "num_timeouts": num_timeouts,
        "time_total_ms": round(sum(time_non_timeouts), 3),
        "time_total_with_timeouts_ms": round(sum(time_all), 3),
        "time_avg_ms": round(mean(time_non_timeouts), 3),
        "time_stdev_ms": round(stdev(time_non_timeouts), 3),
    }

def process_flat_checker_vc(path):
    rows = read_csv(path)

    subjects = set()
    subjects_success = set()
    subjects_failure = set()
    subjects_unknown = set()
    num_goals = len(rows)
    num_sat = 0
    num_unsat = 0
    time_all = []
    time_non_timeouts = []  # no timeouts here by definition

    for row in rows:
        subject = row["subject"]
        subjects.add(subject)
        success = row.get("success", "").lower()
        time_ms = safe_float(row.get("time_ms"))

        if success == "true":
            num_unsat += 1
            subjects_success.add(subject)
        elif success == "false":
            num_sat += 1
            subjects_failure.add(subject)
        else:
            num_subjects_unknown.add(subject)

        if time_ms is not None:
            time_all.append(time_ms)
            time_non_timeouts.append(time_ms)

    num_subjects = len(subjects)
    num_subjects_verified = 0
    num_subjects_refuted = 0
    num_subjects_unknown = 0
    for subject in subjects:
        if subject in subjects_unknown:
            num_subjects_unknown += 1
        elif subject in subjects_failure:
            num_subjects_refuted += 1
        elif subject in subjects_success:
            num_subjects_verified += 1

    return {
        "method": "flat-checker",
        "num_subjects": num_subjects,
        "num_subjects_verified": num_subjects_verified,
        "num_subjects_refuted": num_subjects_refuted,
        "num_subjects_unknown": num_subjects_unknown,        
        "num_goals": num_goals,
        "num_sat": num_sat,
        "num_unsat": num_unsat,
        "num_timeouts": 0,
        "time_total_ms": round(sum(time_non_timeouts), 3),
        "time_total_with_timeouts_ms": round(sum(time_all), 3),
        "time_avg_ms": round(mean(time_non_timeouts), 3),
        "time_stdev_ms": round(stdev(time_non_timeouts), 3),
    }

def main():
    methods_dir = "methods"
    output_rows = []

    for method in os.listdir(methods_dir):
        method_path = os.path.join(methods_dir, method)
        if not os.path.isdir(method_path):
            continue

        if method == "flat-checker":
            vc_path = os.path.join(method_path, results_dir, "results_vc.csv")
            if os.path.isfile(vc_path):
                stats = process_flat_checker_vc(vc_path)
                output_rows.append(stats)
        else:
            results_path = os.path.join(method_path, results_dir, "results.csv")
            if os.path.isfile(results_path):
                stats = process_standard_file(method, results_path)
                output_rows.append(stats)

    # Write final results.csv
    fieldnames = [
        "method", 
        "num_subjects", "num_subjects_verified", "num_subjects_refuted", "num_subjects_unknown",
        "num_goals", "num_sat", "num_unsat", "num_timeouts",
        "time_total_ms", "time_total_with_timeouts_ms", "time_avg_ms", "time_stdev_ms"
    ]
    os.makedirs("results", exist_ok=True)
    with open("results/results_by_method.csv", "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(output_rows)

if __name__ == "__main__":
    main()
