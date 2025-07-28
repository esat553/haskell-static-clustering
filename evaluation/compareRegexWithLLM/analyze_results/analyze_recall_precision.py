import pandas as pd
import os

def calculate_f1_score(tp, fp, fn):
    if tp == 0:
        return 0.0
    p = tp / (tp + fp) if (tp + fp) else 0.0
    r = tp / (tp + fn) if (tp + fn) else 0.0
    return 2 * p * r / (p + r) if (p + r) else 0.0

def get_cluster_metrics(llm_csv_path, comparison_name):
    base_dir = os.path.dirname(__file__)
    manual_path = os.path.abspath(os.path.join(base_dir, '../../../prototype/clustering_results/ghc9_4_8_clustering_results.csv'))
    df_llm = pd.read_csv(llm_csv_path).rename(columns={'detected_cluster': 'llm_cluster', 'submissionID': 'submissionID'})
    df_manual = pd.read_csv(manual_path).rename(columns={'detected_cluster': 'manual_cluster', 'id': 'submissionID'})
    df = pd.merge(df_llm[['submissionID', 'llm_cluster']], df_manual[['submissionID', 'manual_cluster']], on='submissionID', how='outer')
    clusters = pd.concat([df['llm_cluster'], df['manual_cluster']]).dropna().unique()
    rows = []
    for c in sorted(clusters):
        tp = ((df['llm_cluster'] == c) & (df['manual_cluster'] == c)).sum()
        fp = ((df['llm_cluster'] == c) & (df['manual_cluster'] != c)).sum()
        fn = ((df['manual_cluster'] == c) & (df['llm_cluster'] != c)).sum()
        p = tp / (tp + fp) if (tp + fp) else 0.0
        r = tp / (tp + fn) if (tp + fn) else 0.0
        f1 = calculate_f1_score(tp, fp, fn)
        rows.append({"Cluster": c, "Type": comparison_name, "TP": tp, "FP": fp, "FN": fn, "Precision": p, "Recall": r, "F1": f1})
    return pd.DataFrame(rows)

if __name__ == "__main__":
    base_dir = os.path.dirname(__file__)
    llm_code_path = os.path.abspath(os.path.join(base_dir, '../results/llm_categorization_sc.csv'))
    llm_error_path = os.path.abspath(os.path.join(base_dir, '../results/llm_categorization_em.csv'))
    print(get_cluster_metrics(llm_code_path, "LLM Source Code vs. Manual").sort_values("F1", ascending=False).to_string(index=False))
    print(get_cluster_metrics(llm_error_path, "LLM Error Message vs. Manual").sort_values("F1", ascending=False).to_string(index=False))