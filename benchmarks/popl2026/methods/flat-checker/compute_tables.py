import pandas as pd

categories = pd.read_csv("../../subjects/categories.csv")      # subject,category
results = pd.read_csv("results/results.csv")            # subject,time_ms,status,success
results_vc = pd.read_csv("results/results_vc.csv")      # subject,goal,time_ms,success

# Merge categories with results and results_vc
results_merged = pd.merge(results, categories, on='subject', how='left')
results_vc_merged = pd.merge(results_vc, categories, on='subject', how='left')

# Group by category
grouped_results = results_merged.groupby('category')
grouped_vc = results_vc_merged.groupby('category')

agg = pd.DataFrame({
    'num_subjects': grouped_results['subject'].nunique(),
    'num_goals': grouped_vc.size(),               # total rows (goals) per category
    'success_rate': grouped_results['success'].mean(),  # mean() gives fraction of true successes
    'avg_time_ms': grouped_results['time_ms'].mean(),
    'avg_time_stdev': grouped_results['time_ms'].std()
}).fillna(0)

# Compute total row
total_num_subjects = categories['subject'].nunique()
total_num_goals = len(results_vc)
total_success_rate = results['success'].mean()
total_avg_time = results['time_ms'].mean()
total_avg_stdev = results['time_ms'].std()

total_row = pd.DataFrame({
    'num_subjects': [total_num_subjects],
    'num_goals': [total_num_goals],
    'success_rate': [total_success_rate],
    'avg_time_ms': [total_avg_time],
    'avg_time_stdev': [total_avg_stdev]
}, index=['total'])

output = pd.concat([agg, total_row])
output = output.reset_index()
output.to_csv("results/results_by_category.csv", index=False)
