import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- KONFIGURATION ---

# Pfade zu den CSV-Dateien
REAL_CSV = "results/splittability_eval_gemini_2_5_pro.csv"
RANDOM_CSV = "results/splittability_eval_random_gemini_2_5_pro.csv"

OUTPUT_DIR = "analysis_results"

# --- FUNKTIONEN ---

def load_data_from_csv(csv_path: str, experiment_type: str) -> pd.DataFrame:
    """Lädt Daten aus einer CSV-Datei und fügt den Experimententyp hinzu."""
    if not os.path.exists(csv_path):
        print(f"  - WARNUNG: Datei '{csv_path}' nicht gefunden.")
        return pd.DataFrame()
    df = pd.read_csv(csv_path)
    # Nur relevante Spalten und nur Zeilen mit Rating
    df = df[['cluster_label', 'rating']].dropna(subset=['rating'])
    df['experiment_type'] = experiment_type
    # Ratings als int (kann als float gespeichert sein)
    df['rating'] = df['rating'].astype(int)
    return df

def perform_overall_analysis(df: pd.DataFrame):
    print("\n" + "="*50)
    print(" GESAMTANALYSE: ECHTE VS. ZUFÄLLIGE CLUSTER")
    print("="*50)
    df_clean = df.dropna(subset=['rating'])
    summary = df_clean.groupby('experiment_type')['rating'].agg(['mean', 'median', 'std', 'count']).reset_index()
    summary.columns = ['Experiment Typ', 'Mittelwert', 'Median', 'Standardabweichung', 'Anzahl Bewertungen']
    print(summary.to_string(index=False))

    plt.style.use('seaborn-v0_8-whitegrid')
    plt.figure(figsize=(10, 7))
    sns.boxplot(x='experiment_type', y='rating', data=df_clean, palette="viridis",
                width=0.5, boxprops=dict(alpha=.8), whiskerprops=dict(alpha=.8),
                capprops=dict(alpha=.8), medianprops=dict(color="orange", linewidth=2.5))
    sns.stripplot(x='experiment_type', y='rating', data=df_clean, color=".25", size=4, alpha=0.4)
    plt.title('Comparison of Cluster Homogeneity Ratings', fontsize=18, pad=20)
    plt.xlabel('Experiment Type', fontsize=14)
    plt.ylabel('Homogeneity Rating (1 = Highly Splittable, 5 = Perfectly Homogeneous)', fontsize=14)
    plt.xticks(ticks=[0, 1], labels=['Rule-Based Clusters', 'Random Control Group'], fontsize=12)
    plt.yticks(fontsize=12)
    plt.grid(axis='y', linestyle='--', alpha=0.7)
    output_path = os.path.join(OUTPUT_DIR, "homogeneity_boxplot_corrected.png")
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"\n-> Korrigierter Boxplot gespeichert unter: {output_path}")

    print("\n" + "-"*50)
    print(" RATING DISTRIBUTION PER GROUP (Counts)")
    print("-" * 50)
    print(df_clean.groupby('experiment_type')['rating'].value_counts().unstack(fill_value=0))

    print("\n" + "-"*50)
    print(" RATING DISTRIBUTION PER GROUP (Percentages)")
    print("-" * 50)
    print(df_clean.groupby('experiment_type')['rating'].value_counts(normalize=True).unstack(fill_value=0).applymap('{:.2%}'.format))

def perform_cluster_specific_analysis(df: pd.DataFrame):
    # Nur echte Cluster betrachten
    cluster_stats = df[df['experiment_type'] == 'Real'].groupby('cluster_label')['rating'].agg(['mean', 'std', 'count']).reset_index()
    cluster_stats.columns = ['Cluster Label', 'Mean Homogeneity', 'Std Dev', 'Num Evals']
    best_clusters = cluster_stats.sort_values(by='Mean Homogeneity', ascending=False).head(5)
    worst_clusters = cluster_stats.sort_values(by='Mean Homogeneity', ascending=True).head(5)
    print("\n--- TOP 5 BEST CLUSTERS (Highest Homogeneity Score) ---")
    print(best_clusters.to_string(index=False))
    print("\n--- TOP 5 WORST CLUSTERS (Lowest Homogeneity Score / Highest Splittability) ---")
    print(worst_clusters.to_string(index=False))
    best_clusters.to_csv(os.path.join(OUTPUT_DIR, "top5_best_clusters.csv"), index=False)
    worst_clusters.to_csv(os.path.join(OUTPUT_DIR, "top5_worst_clusters.csv"), index=False)
    cluster_stats.to_csv(os.path.join(OUTPUT_DIR, "all_cluster_stats.csv"), index=False)
    print(f"\n-> Detaillierte Cluster-Statistiken gespeichert in: {OUTPUT_DIR}/")

# --- HAUPTPROGRAMM ---

def main():
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
    real_df = load_data_from_csv(REAL_CSV, "Real")
    random_df = load_data_from_csv(RANDOM_CSV, "Random")
    if real_df.empty and random_df.empty:
        print("\nFATAL: Keine Daten aus den angegebenen CSV-Dateien geladen. Skript wird beendet.")
        return
    full_df = pd.concat([real_df, random_df], ignore_index=True)
    df_real = full_df[full_df['experiment_type'] == 'Real']
    df_random = full_df[full_df['experiment_type'] == 'Random']
    df_real['rating'].to_csv('real_ratings.dat', index=False, header=False)
    df_random['rating'].to_csv('random_ratings.dat', index=False, header=False)
    perform_overall_analysis(full_df)
    perform_cluster_specific_analysis(full_df)
    print("\n" + "="*50)
    print("ANALYSE ABGESCHLOSSEN")
    print("="*50)

if __name__ == "__main__":
    main()
