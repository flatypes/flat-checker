import os
import csv

def count_inv_calls(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        return f.read().count("inv(")

def main(folder, output_csv):
    results = []
    for filename in os.listdir(folder):
        if filename.endswith(".py"):
            full_path = os.path.join(folder, filename)
            count = count_inv_calls(full_path)
            subject = os.path.splitext(filename)[0]  # drop .py
            results.append((subject, count))

    results.sort()  # sort by subject

    with open(output_csv, "w", newline="", encoding="utf-8") as out:
        writer = csv.writer(out)
        writer.writerow(["subject", "inv"])
        writer.writerows(results)

if __name__ == "__main__":
    main("../../subjects", "invariants.csv")
