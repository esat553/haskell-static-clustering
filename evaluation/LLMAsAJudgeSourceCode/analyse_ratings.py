import os
import pandas as pd
from tabulate import tabulate
from scipy.stats import kendalltau, pearsonr, ttest_ind

# --- Konfiguration: Mapping von Modellnamen zu CSV-Dateien ---
LLM_CSVS = {
    "Claude 4 Sonnet": "results/llm_as_judge_sonnet_4.csv",
    "Gemini 2.5 Flash": "results/llm_as_judge_gemini_2_5_flash.csv",
    "GPT-4o": "results/llm_as_judge_gpt_4o.csv",
}
RANDOM_CSVS = {
    "Claude 4 Sonnet (Random)": "results/llm_as_judge_sonnet_4_random.csv",
    "Gemini 2.5 Flash (Random)": "results/llm_as_judge_gemini_2_5_flash_random.csv",
    "GPT-4o (Random)": "results/llm_as_judge_gpt_4o_random.csv",
}
RESULTS_DIR = "results"
REPORT_PATH = os.path.join(RESULTS_DIR, "analysis_report.txt")

def load_data(csv_map):
    dataframes = {}
    for name, path in csv_map.items():
        if not os.path.exists(path):
            print(f"[WARN] Datei nicht gefunden: {path}")
            continue
        df = pd.read_csv(path)
        df = df[df['rating'].isin([1,2,3,4,5])]
        dataframes[name] = df
    return dataframes

def analyze_ratings(dataframes, report_lines, title):
    report_lines.append(f"\n{'='*80}\n{title}\n{'='*80}")
    for name, df in dataframes.items():
        valid = df[df['rating'].isin([1,2,3,4,5])]
        if valid.empty:
            report_lines.append(f"\n{name}: Keine gültigen Bewertungen.")
            continue
        report_lines.append(f"\n{name}:")
        report_lines.append(f"  Anzahl: {len(valid)}")
        report_lines.append(f"  Durchschnitt: {valid['rating'].mean():.2f}")
        report_lines.append(f"  Standardabw.: {valid['rating'].std():.2f}")
        report_lines.append("  Verteilung:")
        for v in range(5,0,-1):
            c = (valid['rating']==v).sum()
            p = c/len(valid)*100
            report_lines.append(f"    {v}: {c:>5} ({p:.1f}%)")

def analyze_clusters(dataframes, report_lines, title):
    report_lines.append(f"\n{'-'*40}\nCluster-Auswertung: {title}\n{'-'*40}")
    for name, df in dataframes.items():
        avg_per_cluster = df.groupby('cluster_label')['rating'].agg(['mean', 'count']).reset_index()
        avg_per_cluster.columns = ['Cluster Label', 'Avg Rating', 'Num Samples']
        avg_per_cluster = avg_per_cluster.sort_values(by='Avg Rating', ascending=False)
        report_lines.append(f"\n{name}:\n" + tabulate(avg_per_cluster, headers='keys', tablefmt='psql', showindex=False, floatfmt=".2f"))

def analyze_cross_model(llm_data, report_lines):
    keys = list(llm_data.keys())
    if len(keys) < 2:
        return
    merged = None
    for k in keys:
        col = llm_data[k][['submissionID','rating']].rename(columns={'rating':k})
        merged = col if merged is None else merged.merge(col, on='submissionID')
    merged = merged[merged[keys].isin([1,2,3,4,5]).all(axis=1)]
    report_lines.append(f"\n{'-'*40}\nModellübergreifende Analyse\n{'-'*40}")
    report_lines.append(f"Gemeinsame Einreichungen: {len(merged)}")
    if len(keys) == 3:
        agree = merged[
            ((merged[keys[0]] - merged[keys[1]]).abs() <= 1) &
            ((merged[keys[0]] - merged[keys[2]]).abs() <= 1) &
            ((merged[keys[1]] - merged[keys[2]]).abs() <= 1)
        ]
        report_lines.append(f"Enge Übereinstimmung (<=1): {len(agree)} ({len(agree)/len(merged)*100:.1f}%)")
    report_lines.append("\nKorrelationen (Kendall tau, Pearson r):")
    for i in range(len(keys)):
        for j in range(i+1, len(keys)):
            a, b = keys[i], keys[j]
            tau, pt = kendalltau(merged[a], merged[b])
            r, pr = pearsonr(merged[a], merged[b])
            report_lines.append(f"{a} vs {b}: tau={tau:.3f} (p={pt:.4f}), r={r:.3f} (p={pr:.4f})")

def analyze_random_baseline(llm_data, random_data, report_lines):
    report_lines.append(f"\n{'-'*40}\nRandom-Baseline-Analyse\n{'-'*40}")
    report_lines.append(f"{'Modell':<25} | {'n (True)':<8} | {'n (Rand)':<8} | {'Mean (True)':<10} | {'Mean (Rand)':<10} | {'Δ Mean':<8} | {'t':<8} | {'p':<8}")
    for k in llm_data:
        true = llm_data[k]['rating'].dropna()
        rand_key = [rk for rk in random_data if k in rk or rk.startswith(k.split()[0])]
        if not rand_key:
            report_lines.append(f"{k:<25} | {'-':<8} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<8} | {'-':<8} | {'-':<8}")
            continue
        rand = random_data[rand_key[0]]['rating'].dropna()
        true = true[true.isin([1,2,3,4,5])]
        rand = rand[rand.isin([1,2,3,4,5])]
        if rand.empty:
            report_lines.append(f"{k:<25} | {'-':<8} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<8} | {'-':<8} | {'-':<8}")
            continue
        mt, mr = true.mean(), rand.mean()
        t, p = ttest_ind(true, rand, equal_var=False)
        report_lines.append(f"{k:<25} | {len(true):<8} | {len(rand):<8} | {mt:<10.2f} | {mr:<10.2f} | {mt-mr:<8.2f} | {t:<8.2f} | {p:<8.2g}")

def main():
    os.makedirs(RESULTS_DIR, exist_ok=True)
    report_lines = []
    llm_data = load_data(LLM_CSVS)
    random_data = load_data(RANDOM_CSVS)
    analyze_ratings(llm_data, report_lines, "LLM-Bewertungen")
    analyze_ratings(random_data, report_lines, "Random-Baseline")
    analyze_clusters(llm_data, report_lines, "LLM-Bewertungen")
    analyze_clusters(random_data, report_lines, "Random-Baseline")
    analyze_cross_model(llm_data, report_lines)
    analyze_random_baseline(llm_data, random_data, report_lines)
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write("\n".join(report_lines))
    print(f"\nAnalyse abgeschlossen. Ergebnisse gespeichert in: {REPORT_PATH}\n")
    print("\n".join(report_lines[:40]))
    print("... (vollständiger Bericht siehe .txt)")

if __name__ == "__main__":
    main()
