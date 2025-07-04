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

    num_goals = len(rows)
    num_sat = 0
    num_unsat = 0
    num_timeouts = 0
    time_all = []
    time_non_timeouts = []

    for row in rows:
        result = row.get("result", "").lower()
        time_ms = safe_float(row.get("time_ms"))

        if result == "sat":
            num_sat += 1
        elif result == "unsat":
            num_unsat += 1
        elif result == "timeout":
            num_timeouts += 1

        if time_ms is not None:
            time_sec = time_ms / 1000
            time_all.append(time_sec)
            if result != "timeout":
                time_non_timeouts.append(time_sec)

    return {
        "method": method,
        "num_goals": num_goals,
        "num_sat": num_sat,
        "num_unsat": num_unsat,
        "num_timeouts": num_timeouts,
        "time_total_sec": round(sum(time_non_timeouts), 3),
        "time_total_with_timeouts_sec": round(sum(time_all), 3),
        "time_avg_sec": round(mean(time_non_timeouts), 3),
        "time_stdev_sec": round(stdev(time_non_timeouts), 3),
    }

def process_flat_checker_vc(path):
    rows = read_csv(path)

    num_goals = len(rows)
    num_sat = 0
    num_unsat = 0
    time_all = []
    time_non_timeouts = []  # no timeouts here by definition

    for row in rows:
        success = row.get("success", "").lower()
        time_ms = safe_float(row.get("time_ms"))

        if success == "true":
            num_unsat += 1
        elif success == "false":
            num_sat += 1

        if time_ms is not None:
            time_sec = time_ms / 1000
            time_all.append(time_sec)
            time_non_timeouts.append(time_sec)

    return {
        "method": "flat-checker",
        "num_goals": num_goals,
        "num_sat": num_sat,
        "num_unsat": num_unsat,
        "num_timeouts": 0,
        "time_total_sec": round(sum(time_non_timeouts), 3),
        "time_total_with_timeouts_sec": round(sum(time_all), 3),
        "time_avg_sec": round(mean(time_non_timeouts), 3),
        "time_stdev_sec": round(stdev(time_non_timeouts), 3),
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
        "method", "num_goals", "num_sat", "num_unsat", "num_timeouts",
        "time_total_sec", "time_total_with_timeouts_sec", "time_avg_sec", "time_stdev_sec"
    ]
    os.makedirs("results", exist_ok=True)
    with open("results/results_by_method.csv", "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(output_rows)

if __name__ == "__main__":
    main()
