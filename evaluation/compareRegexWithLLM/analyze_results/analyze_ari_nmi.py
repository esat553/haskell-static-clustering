import pandas as pd
from sklearn.metrics import adjusted_rand_score, normalized_mutual_info_score
import os

GENERALIZATION_MAP = {
    # Parse- & Syntax-Fehler
    'GHCi Kontext in Abgabe': 'Parse-Fehler',
    'Ungültige Top-Level-Deklaration': 'Parse-Fehler',
    'Parse-Fehler durch Import-Fehler': 'Parse-Fehler',
    'Parse-Fehler in \'let\'-Binding': 'Parse-Fehler',
    'Parse-Fehler in Funktionsdeklaration': 'Parse-Fehler',
    'Parse-Fehler': 'Parse-Fehler',
    'Lexikalischer Fehler': 'Parse-Fehler',
    'Ungültiges Zeichen': 'Parse-Fehler',
    'Leerer do-Block': 'Parse-Fehler',
    'Letzte Anweisung im \'do\'-Block': 'Parse-Fehler',
    'Fehlende Klammern im Range-Ausdruck': 'Parse-Fehler',
    'Fehlerhafter Typ-Header': 'Parse-Fehler',
    'Ungültige Binding-Syntax': 'Parse-Fehler',

    # Typenkonflikte & Art-Fehler
    'Falsche Funktionsarität': 'Typenkonflikt',
    'Inkonsistenter Rückgabetyp': 'Typenkonflikt',
    'Implementierung verletzt Typsignatur': 'Typenkonflikt',
    'Numerischer Typenkonflikt': 'Typenkonflikt',
    'Typenkonflikt': 'Typenkonflikt',
    'Unendlicher Typ': 'Typenkonflikt',
    'Kind-Konflikt': 'Typenkonflikt',
    'Kind-Konflikt (Constraint vs. Typ)': 'Typenkonflikt',
    'Constraint erwartet, aber Typ erhalten': 'Typenkonflikt',
    'Falsche Anzahl von Typ-Argumenten': 'Typenkonflikt',
    'Falsche Arität für Konstruktor': 'Typenkonflikt',
    'Abweichende Arity': 'Typenkonflikt',
    'Typed Hole': 'Typenkonflikt',

    # Gültigkeitsbereich
    'Typenkonstruktor oder Klasse nicht definiert': 'Variable nicht im Gültigkeitsbereich',
    'Nicht definierter Datenkonstruktor': 'Variable nicht im Gültigkeitsbereich',
    'Funktion nicht definiert': 'Variable nicht im Gültigkeitsbereich',
    'Variable nicht im Gültigkeitsbereich': 'Variable nicht im Gültigkeitsbereich',
    'Modul nicht gefunden': 'Variable nicht im Gültigkeitsbereich',
    'Methode nicht in Klasse': 'Variable nicht im Gültigkeitsbereich',

    # Konfliktierende & fehlende Definitionen/Bindings
    'Doppelte Signatur': 'Konfliktierende Definitionen',
    'Mehrfache Deklarationen': 'Konfliktierende Definitionen',
    'Konflikt in \'data\'-Deklaration': 'Konfliktierende Definitionen',
    'Mehrfachdefinition in Funktionsgleichung': 'Konfliktierende Definitionen',
    'Konfliktierende Bindings': 'Konfliktierende Definitionen',
    'Fehlendes Binding': 'Konfliktierende Definitionen',
    'Pattern Binding in Instanz': 'Konfliktierende Definitionen',
    'Mehrfache Instanzdeklaration': 'Konfliktierende Definitionen',
    'Überlappende Instanzen': 'Konfliktierende Definitionen',

    # Fehlende Instanzen & Constraints
    'Fehlende Constraint bei Funktionssignatur': 'Fehlende Instanz / Constraint',
    'Fehlende Superklassen-Instanz': 'Fehlende Instanz / Constraint',
    'Fehlende Instanz bei \'deriving\'': 'Fehlende Instanz / Constraint',
    'Fehlende Instanz': 'Fehlende Instanz / Constraint',
    'Constraint nicht erfüllbar': 'Fehlende Instanz / Constraint',
    'Flexible Kontexte benötigt': 'Fehlende Instanz / Constraint',
    'Fehlende GADTs-Erweiterung': 'Fehlende Instanz / Constraint',

    # Mehrdeutigkeit
    'Mehrdeutiger Typ': 'Mehrdeutigkeit',
    'Mehrdeutiger Bezeichner': 'Mehrdeutigkeit',

    # Ungültige Deklarationen
    'Ungültiger Typ-Operator': 'Ungültige Deklaration',
    'Ungültige Instanz-Signatur': 'Ungültige Deklaration',
    'Ungültige Typensignatur': 'Ungültige Deklaration',
    'Ungültige Instanz-Form': 'Ungültige Deklaration',
    'Fehler mit Datenkonstruktoren': 'Ungültige Deklaration',
    'Ungültiges Enum-Deriving': 'Ungültige Deklaration',
    'Ungültiges Deriving': 'Ungültige Deklaration',
}

def generalize_cluster(label):
    return GENERALIZATION_MAP.get(label, label)

base_dir = os.path.dirname(__file__)
regex_path = os.path.abspath(os.path.join(base_dir, '../../../prototype/clustering_results/ghc9_4_8_clustering_results.csv'))
llm_error_path = os.path.abspath(os.path.join(base_dir, '../results/llm_categorization_em.csv'))
llm_code_path = os.path.abspath(os.path.join(base_dir, '../results/llm_categorization_sc.csv'))

df_regex = pd.read_csv(regex_path)
df_llm_error = pd.read_csv(llm_error_path)
df_llm_code = pd.read_csv(llm_code_path)


df_regex = df_regex.rename(columns={'id': 'submissionID', 'detected_cluster': 'c_regex'})
df = df_regex[['submissionID', 'c_regex']].copy()

df = df.merge(df_llm_error[['submissionID', 'detected_cluster']], on='submissionID', how='inner')
df = df.rename(columns={'detected_cluster': 'c_llm_error'})

df = df.merge(df_llm_code[['submissionID', 'detected_cluster']], on='submissionID', how='inner')
df = df.rename(columns={'detected_cluster': 'c_llm_code'})

print(df.columns)

labels_regex_fine = df["c_regex"].tolist()
labels_llm_error_fine = df["c_llm_error"].tolist()
labels_llm_code_fine = df["c_llm_code"].tolist()

df['c_regex_gen'] = df['c_regex'].apply(generalize_cluster)
df['c_llm_error_gen'] = df['c_llm_error'].apply(generalize_cluster)
df['c_llm_code_gen'] = df['c_llm_code'].apply(generalize_cluster)

labels_regex_gen = df['c_regex_gen'].tolist()
labels_llm_error_gen = df['c_llm_error_gen'].tolist()
labels_llm_code_gen = df['c_llm_code_gen'].tolist()


ari_regex_code_fine = adjusted_rand_score(labels_regex_fine, labels_llm_code_fine)
nmi_regex_code_fine = normalized_mutual_info_score(labels_regex_fine, labels_llm_code_fine)
ari_regex_error_fine = adjusted_rand_score(labels_regex_fine, labels_llm_error_fine)
nmi_regex_error_fine = normalized_mutual_info_score(labels_regex_fine, labels_llm_error_fine)
ari_llm_error_code_fine = adjusted_rand_score(labels_llm_error_fine, labels_llm_code_fine)
nmi_llm_error_code_fine = normalized_mutual_info_score(labels_llm_error_fine, labels_llm_code_fine)

ari_regex_code_gen = adjusted_rand_score(labels_regex_gen, labels_llm_code_gen)
nmi_regex_code_gen = normalized_mutual_info_score(labels_regex_gen, labels_llm_code_gen)
ari_regex_error_gen = adjusted_rand_score(labels_regex_gen, labels_llm_error_gen)
nmi_regex_error_gen = normalized_mutual_info_score(labels_regex_gen, labels_llm_error_gen)
ari_llm_error_code_gen = adjusted_rand_score(labels_llm_error_gen, labels_llm_code_gen)
nmi_llm_error_code_gen = normalized_mutual_info_score(labels_llm_error_gen, labels_llm_code_gen)

print("--- Ergebnisse (Feingranulare Cluster) ---")
print("=" * 45)
print(f"{'Vergleichspaar':<30} | {'ARI':>5} | {'NMI':>5}")
print("-" * 45)
print(f"{'Regex vs. LLM (Code)':<30} | {ari_regex_code_fine:>5.3f} | {nmi_regex_code_fine:>5.3f}")
print(f"{'Regex vs. LLM (Error)':<30} | {ari_regex_error_fine:>5.3f} | {nmi_regex_error_fine:>5.3f}")
print(f"{'LLM (Error) vs. LLM (Code)':<30} | {ari_llm_error_code_fine:>5.3f} | {nmi_llm_error_code_fine:>5.3f}")
print("=" * 45)

print("\n")
print("--- Ergebnisse (Generalisierte Cluster) ---")
print("=" * 45)
print(f"{'Vergleichspaar':<30} | {'ARI':>5} | {'NMI':>5}")
print("-" * 45)
print(f"{'Regex vs. LLM (Code)':<30} | {ari_regex_code_gen:>5.3f} | {nmi_regex_code_gen:>5.3f}")
print(f"{'Regex vs. LLM (Error)':<30} | {ari_regex_error_gen:>5.3f} | {nmi_regex_error_gen:>5.3f}")
print(f"{'LLM (Error) vs. LLM (Code)':<30} | {ari_llm_error_code_gen:>5.3f} | {nmi_llm_error_code_gen:>5.3f}")
print("=" * 45)
