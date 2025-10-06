import csv
import json
import os
import math
from collections import defaultdict

# ==== CONFIGURABLE PATHS ====
CATEGORIES_CSV = "../../subjects/categories.csv"
INVARIANTS_CSV = "invariants.csv"
RESULTS_CSV = "results/results.csv"
SUBJECT_JSON_DIR = "results/"
OUTPUT_CSV = "results/results_by_category.csv"

# ==== LOAD CATEGORY MAPPINGS ====
subject_to_category = {}
with open(CATEGORIES_CSV) as f:
    reader = csv.DictReader(f)
    for row in reader:
        subject_to_category[row["subject"]] = row["category"]

# ==== LOAD INVARIANTS ====
inv_count = defaultdict(int)
with open(INVARIANTS_CSV) as f:
    reader = csv.DictReader(f)
    for row in reader:
        subj = row["subject"]
        inv_count[subj] += int(row["inv"])

# ==== LOAD OVERALL RESULTS ====
results = {}
with open(RESULTS_CSV) as f:
    reader = csv.DictReader(f)
    for row in reader:
        subj = row["subject"]
        success = row["success"].lower() == "true"
        time_ms = float(row["time_ms"])
        results[subj] = {"success": success, "time_ms": time_ms}

# ==== AGGREGATION STRUCTURE ====
agg = defaultdict(lambda: {
    "subjects": set(),
    "num_inv": 0,
    "num_verified": 0,
    "num_goals": 0,
    "num_trivial": 0,
    "num_smt_simp": 0,
    "num_smt_lemmas": 0,
    "num_lemmas": 0,
    "times": []
})

# ==== PROCESS EACH SUBJECT ====
for subject, category in subject_to_category.items():
    agg[category]["subjects"].add(subject)
    agg[category]["num_inv"] += inv_count.get(subject, 0)

    if subject in results:
        res = results[subject]
        if res["success"]:
            agg[category]["num_verified"] += 1
        agg[category]["times"].append(res["time_ms"])

    json_path = os.path.join(SUBJECT_JSON_DIR, f"{subject}.json")
    if not os.path.isfile(json_path):
        continue

    with open(json_path) as jf:
        try:
            data = json.load(jf)[0]["goals"]
        except Exception:
            continue

    for goal in data:
        agg[category]["num_goals"] += 1
        use_smt = goal.get("use SMT", False)
        use_lemmas = goal.get("use lemmas", False)

        if not use_smt and not use_lemmas:
            agg[category]["num_trivial"] += 1
        elif use_smt and not use_lemmas:
            agg[category]["num_smt_simp"] += 1
        elif use_smt and use_lemmas:
            agg[category]["num_smt_lemmas"] += 1
        elif not use_smt and use_lemmas:
            agg[category]["num_lemmas"] += 1

# ==== AGGREGATE TOTALS ====
all_subjects = set()
for cat, data in agg.items():
    if cat != "total":
        all_subjects.update(data["subjects"])

agg["total"] = {
    "subjects": all_subjects,
    "num_inv": sum(agg[cat]["num_inv"] for cat in agg if cat != "total"),
    "num_verified": sum(agg[cat]["num_verified"] for cat in agg if cat != "total"),
    "num_goals": sum(agg[cat]["num_goals"] for cat in agg if cat != "total"),
    "num_trivial": sum(agg[cat]["num_trivial"] for cat in agg if cat != "total"),
    "num_smt_simp": sum(agg[cat]["num_smt_simp"] for cat in agg if cat != "total"),
    "num_smt_lemmas": sum(agg[cat]["num_smt_lemmas"] for cat in agg if cat != "total"),
    "num_lemmas": sum(agg[cat]["num_lemmas"] for cat in agg if cat != "total"),
    "times": [t for cat in agg if cat != "total" for t in agg[cat]["times"]],
}

# ==== ENSURE OUTPUT DIRECTORY EXISTS ====
output_dir = os.path.dirname(OUTPUT_CSV)
if output_dir and not os.path.exists(output_dir):
    os.makedirs(output_dir, exist_ok=True)

# ==== WRITE OUTPUT CSV ====
with open(OUTPUT_CSV, "w", newline="") as out:
    writer = csv.writer(out)
    writer.writerow(["category", "num_subjects", "num_inv", "num_verified",
                     "num_goals", "num_trivial", "num_smt_simp",
                     "num_smt_lemmas", "num_lemmas", "time_ms_avg", "time_ms_stdev"])

    for cat, data in agg.items():
        times = data["times"]
        n = len(times)
        avg = sum(times) / n if n else 0.0
        stdev = math.sqrt(sum((t - avg) ** 2 for t in times) / n) if n else 0.0
        writer.writerow([
            cat,
            len(data["subjects"]),
            data["num_inv"],
            data["num_verified"],
            data["num_goals"],
            data["num_trivial"],
            data["num_smt_simp"],
            data["num_smt_lemmas"],
            data["num_lemmas"],
            f"{avg:.2f}",
            f"{stdev:.2f}"
        ])
