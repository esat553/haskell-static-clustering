import os
import sys
import re
import numpy as np
import pandas as pd
from pathlib import Path
from tqdm import tqdm
import streamlit as st
import plotly.express as px
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE

# Tree-sitter & AST
from tree_sitter import Language, Parser
import networkx as nx

# =============================================================================
# KONFIGURATION
# =============================================================================
NUM_CLUSTERS = 10  # <--- Clusteranzahl hier festlegen
LATENT_DIM = 16    # Für spätere Erweiterung (z.B. CMVAE)
OUTPUT_DIR = "combined_clustering_results"  # Alle Outputs landen hier
os.makedirs(OUTPUT_DIR, exist_ok=True)

CSV_PATH = "../../../compilation/compilation_results/ghc9_4_8_results.csv"
EMBEDDING_FILE = "embedding_results/embeddings_combined.npy"
EMBEDDING_META = "embedding_results/embeddings_combined.parquet"

# =============================================================================
# AST-FEATURES (wie im ast_experiment.py)
# =============================================================================
def setup_tree_sitter():
    THIS_DIR = Path.cwd()
    haskell_grammar_path = None
    for potential_path in [
        THIS_DIR / "tree-sitter-haskell",
        THIS_DIR.parent / "tree-sitter-haskell",
        THIS_DIR / "../tree-sitter-haskell"
    ]:
        if potential_path.exists():
            haskell_grammar_path = potential_path.resolve()
            break
    if haskell_grammar_path is None:
        raise RuntimeError("tree-sitter-haskell directory not found.")
    PROJECT_ROOT = haskell_grammar_path.parent
    BUILD_DIR = PROJECT_ROOT / "build"
    BUILD_DIR.mkdir(exist_ok=True)
    LANG_SO = BUILD_DIR / "my-languages.so"
    if not LANG_SO.exists():
        Language.build_library(str(LANG_SO), [str(haskell_grammar_path)])
    HASKELL_LANGUAGE = Language(str(LANG_SO), "haskell")
    parser = Parser()
    parser.set_language(HASKELL_LANGUAGE)
    return parser

def clean_haskell_code_for_embedding(code: str) -> str:
    if not isinstance(code, str): return ""
    code = re.sub(r'\{-(.|\n)*?-\}', '', code)
    code = re.sub(r'--.*', '', code)
    code = re.sub(r'\s+', ' ', code).strip()
    return code

node_counter = 0
def ast_to_graph(node, graph, parent_id=None):
    global node_counter
    current_id = node_counter
    node_counter += 1
    graph.add_node(current_id, type=node.type)
    if parent_id is not None:
        graph.add_edge(parent_id, current_id)
    for child in node.children:
        ast_to_graph(child, graph, parent_id=current_id)

def build_graph_from_code(code: str, parser) -> nx.Graph:
    global node_counter
    node_counter = 0
    tree = parser.parse(bytes(code, "utf8"))
    graph = nx.DiGraph()
    ast_to_graph(tree.root_node, graph)
    return graph

HASKELL_NODE_TYPES = [
    'function', 'signature', 'if', 'case', 'case_alternative', 'let', 'where',
    'guard', 'do', 'infix', 'apply', 'variable', 'literal', 'operator',
    'constructor', 'module', 'import'
]

def graph_to_feature_vector(graph: nx.Graph) -> np.ndarray:
    num_features_per_type = 4
    if graph.number_of_nodes() == 0:
        return np.zeros(len(HASKELL_NODE_TYPES) * num_features_per_type)
    G_undirected = graph.to_undirected()
    try:
        betweenness = nx.betweenness_centrality(graph)
        closeness = nx.closeness_centrality(graph)
        degree = nx.degree_centrality(graph)
        eigenvector = nx.eigenvector_centrality_numpy(G_undirected)
    except Exception:
        return np.zeros(len(HASKELL_NODE_TYPES) * num_features_per_type)
    nodes_by_type = {nt: [] for nt in HASKELL_NODE_TYPES}
    for node_id, data in graph.nodes(data=True):
        if data['type'] in nodes_by_type:
            nodes_by_type[data['type']].append(node_id)
    feature_vector = []
    for node_type in HASKELL_NODE_TYPES:
        node_ids = nodes_by_type[node_type]
        if not node_ids:
            feature_vector.extend([0.0] * num_features_per_type)
            continue
        avg_betweenness = np.mean([betweenness.get(nid, 0) for nid in node_ids])
        avg_closeness = np.mean([closeness.get(nid, 0) for nid in node_ids])
        avg_degree = np.mean([degree.get(nid, 0) for nid in node_ids])
        avg_eigenvector = np.mean([eigenvector.get(nid, 0) for nid in node_ids])
        feature_vector.extend([avg_betweenness, avg_closeness, avg_degree, avg_eigenvector])
    return np.array(feature_vector)

# =============================================================================
# PIPELINE: FEATURES EXTRAHIEREN UND SPEICHERN
# =============================================================================
def create_embeddings_if_needed():
    if os.path.exists(EMBEDDING_FILE) and os.path.exists(EMBEDDING_META):
        print("✅ Embedding-Dateien bereits vorhanden, überspringe Erstellung.")
        return
    print("[*] Erstelle Embeddings wie im embedding_experiment.py ...")
    # Korrigiere den Pfad auf embedding/sourceCode
    embedding_exp_path = Path(__file__).resolve().parent.parent / "embedding" / "sourceCode"
    sys.path.append(str(embedding_exp_path))
    try:
        from embedding_experiment import CONFIG as EMB_CFG, load_and_prepare_data, generate_embeddings
    except ImportError:
        raise RuntimeError("embedding_experiment.py konnte nicht importiert werden. Stelle sicher, dass es existiert und korrekt ist.")
    # Passe die Pfade im Embedding-Config an
    EMB_CFG = EMB_CFG.copy()
    EMB_CFG['FILES'] = EMB_CFG['FILES'].copy()
    EMB_CFG['DATA_SOURCE'] = EMB_CFG['DATA_SOURCE'].copy()
    EMB_CFG['FILES']['OUTPUT_DIR'] = "embedding_results"
    EMB_CFG['FILES']['prepared_data'] = "embedding_results/prepared_for_embedding_combined.csv"
    EMB_CFG['FILES']['embeddings_npy'] = EMBEDDING_FILE
    EMB_CFG['FILES']['embeddings_df'] = EMBEDDING_META
    EMB_CFG['DATA_SOURCE']['CSV_PATH'] = CSV_PATH
    os.makedirs(EMB_CFG['FILES']['OUTPUT_DIR'], exist_ok=True)
    load_and_prepare_data(EMB_CFG)
    generate_embeddings(EMB_CFG)
    print("✅ Embeddings wurden wie im embedding_experiment.py erstellt.")

def extract_and_save_features():
    print("[*] Lade CSV...")
    df = pd.read_csv(CSV_PATH)
    if 'id' not in df.columns or 'source_code' not in df.columns:
        raise ValueError("CSV muss 'id' und 'source_code' enthalten.")
    df['clean_code'] = df['source_code'].apply(clean_haskell_code_for_embedding)
    parser = setup_tree_sitter()
    print("[*] Extrahiere AST-Features...")
    ast_features = []
    for code in tqdm(df['clean_code'], desc="AST-Features"):
        g = build_graph_from_code(code, parser)
        v = graph_to_feature_vector(g)
        ast_features.append(v)
    X1 = np.vstack(ast_features)
    np.save(os.path.join(OUTPUT_DIR, "X1_structural.npy"), X1)
    print(f"✅ AST-Features gespeichert: {X1.shape}")

    print("[*] Prüfe Embedding-Features...")
    create_embeddings_if_needed()
    print("[*] Lade Embedding-Features...")
    X2 = np.load(EMBEDDING_FILE)
    meta = pd.read_parquet(EMBEDDING_META)
    # Nutze exakt das gleiche Mapping wie im embedding_experiment.py (combined_text)
    code2idx = {row['combined_text']: i for i, row in meta.iterrows()}
    embedding_features = []
    # Erzeuge combined_text wie im embedding_experiment.py
    def get_clean_code_and_error(code, error):
        import re
        clean_code = re.sub(r'--.*', '', str(code))
        clean_code = re.sub(r'{-(.|\n)*?-}', '', clean_code, flags=re.DOTALL).strip()
        error_blocks = re.split(r'\n(?=.*?:\d+:\d+: error:)', str(error))
        def process_single_error_block(block):
            lines = [line for line in block.strip().split('\n')]
            core_errors, code_context_line, found_error_header = [], "", False
            for i, line in enumerate(lines):
                stripped_line = line.strip()
                if not stripped_line:
                    continue
                if found_error_header:
                    found_error_header = False
                    if not stripped_line.startswith(('•', '|')):
                        core_errors.append(stripped_line)
                    continue
                if 'error:' in line:
                    message_part = line.split('error:', 1)[1].strip()
                    if message_part:
                        core_errors.append(message_part)
                    else:
                        found_error_header = True
                    continue
                if stripped_line.startswith('•'):
                    core_errors.append(stripped_line.lstrip('• ').strip())
                    continue
                if core_errors and (line.startswith(('    ', '\t'))) and not stripped_line.startswith('|'):
                    core_errors[-1] += ' ' + stripped_line
                    continue
                if "parse error" in block.lower() and stripped_line.startswith('|') and '^' in stripped_line:
                    if i > 0 and lines[i-1].strip().startswith('|'):
                        if match := re.search(r'\|\s*(.*)', lines[i-1]):
                            code_context_line = match.group(1).strip()
            if not core_errors:
                return ""
            cleaned_errors = [re.sub(r'\s*::\s*.*$', '', error).strip() for error in core_errors]
            final_error_string = ". ".join(cleaned_errors)
            if "parse error" in final_error_string.lower() and code_context_line:
                final_error_string += f" AT LINE: {code_context_line}"
            return final_error_string
        processed_errors = [msg for block in error_blocks if block.strip() and (msg := process_single_error_block(block))]
        final_error_message = ". ".join(processed_errors)
        if not final_error_message:
            final_error_message = re.sub(r'\s*\n\s*', ' ', str(error)).strip()
        return f'[HASKELL CODE]\n{clean_code}\n[COMPILER ERROR]\n{final_error_message.strip()}'
    for _, row in df.iterrows():
        combined_text = get_clean_code_and_error(row['source_code'], row.get('compiler_output', ''))
        idx = code2idx.get(combined_text, None)
        if idx is not None:
            embedding_features.append(X2[idx])
        else:
            embedding_features.append(np.zeros(X2.shape[1]))
    X2_final = np.vstack(embedding_features)
    np.save(os.path.join(OUTPUT_DIR, "X2_semantic.npy"), X2_final)
    df[['id', 'source_code', 'compiler_output', 'clean_code']].to_csv(os.path.join(OUTPUT_DIR, "submission_ids.csv"), index=False)
    print(f"✅ Embedding-Features gespeichert: {X2_final.shape}")

# =============================================================================
# CLUSTERING & VISUALISIERUNG
# =============================================================================
def run_clustering_and_save():
    X1 = np.load(os.path.join(OUTPUT_DIR, "X1_structural.npy"))
    X2 = np.load(os.path.join(OUTPUT_DIR, "X2_semantic.npy"))
    df = pd.read_csv(os.path.join(OUTPUT_DIR, "submission_ids.csv"))
    print("[*] Führe KMeans-Clustering auf kombinierten Features durch...")
    X = np.concatenate([X1, X2], axis=1)
    kmeans = KMeans(n_clusters=NUM_CLUSTERS, n_init=10, random_state=0)
    labels = kmeans.fit_predict(X)
    df["combined_cluster"] = labels
    np.save(os.path.join(OUTPUT_DIR, "final_cluster_labels.npy"), labels)
    df.to_csv(os.path.join(OUTPUT_DIR, "final_results.csv"), index=False)
    print(f"✅ Clusterlabels gespeichert ({np.bincount(labels)})")

    # t-SNE für Visualisierung
    if len(X) > 2:
        perplexity = max(5, min(30, len(X) // 3))
        tsne = TSNE(n_components=2, perplexity=perplexity, metric="cosine", random_state=0)
        coords = tsne.fit_transform(X)
        np.save(os.path.join(OUTPUT_DIR, "final_latent_vectors.npy"), coords)
        df["x"], df["y"] = coords[:, 0], coords[:, 1]
    else:
        df["x"] = 0.0
        df["y"] = 0.0
    df.to_csv(os.path.join(OUTPUT_DIR, "final_results_with_coords.csv"), index=False)
    print("✅ t-SNE-Koordinaten gespeichert.")

# =============================================================================
# STREAMLIT DASHBOARD
# =============================================================================
def run_dashboard():
    st.set_page_config(page_title="Combined Clustering Dashboard", layout="wide")
    st.title("Kombiniertes Clustering: AST + Embedding Features")
    df = pd.read_csv(os.path.join(OUTPUT_DIR, "final_results_with_coords.csv"))
    st.sidebar.header("Cluster Filter")
    cluster_values = sorted(df["combined_cluster"].unique())
    cluster_options = ["Alle"] + cluster_values
    cluster_choice = st.sidebar.selectbox("Cluster auswählen", cluster_options)
    if cluster_choice != "Alle":
        df_view = df[df["combined_cluster"] == cluster_choice]
    else:
        df_view = df
    st.markdown("### t-SNE-Projektion")
    if "x" in df.columns:
        fig = px.scatter(
            df_view,
            x="x",
            y="y",
            color=df_view["combined_cluster"].astype(str),
            hover_data=["id"],
            title=f"Cluster: {cluster_choice}",
            height=650,
        )
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.info("Weniger als drei Submissions – keine Projektion.")
    st.markdown("### Cluster-Größen")
    size_series = df["combined_cluster"].value_counts().sort_index()
    st.bar_chart(size_series)
    st.markdown(f"### Code-Beispiele – Cluster {cluster_choice}")
    max_examples = 10
    if len(df_view) == 0:
        st.info("Keine Submissions in diesem Cluster.")
    else:
        sample_df = df_view.sample(min(max_examples, len(df_view)), random_state=1)
        for _, row in sample_df.iterrows():
            with st.expander(f"Submission {row['id']}"):
                code = row.get("source_code", "[Code nicht verfügbar]")
                st.code(code, language="haskell")
                if "compiler_output" in row and isinstance(row["compiler_output"], str):
                    st.markdown("**Compiler Error:**")
                    st.code(row["compiler_output"], language="text")

# =============================================================================
# MAIN
# =============================================================================
def run_all_steps():
    print("=== Kombiniertes Clustering: AST + Embedding Features ===")
    print("1. Feature-Extraktion (AST & Embedding)")
    extract_and_save_features()
    print("\n2. Clustering & t-SNE")
    run_clustering_and_save()
    print("\n3. Zusammenfassung:")
    df = pd.read_csv(os.path.join(OUTPUT_DIR, "final_results.csv"))
    labels = np.load(os.path.join(OUTPUT_DIR, "final_cluster_labels.npy"))
    print(f"  Anzahl Submissions: {len(df)}")
    print(f"  Cluster-Verteilung: {np.bincount(labels)}")
    print(f"  Ergebnisse gespeichert in: {OUTPUT_DIR}")
    print("\n4. Beispiel-Cluster-Zuordnung (erste 5):")
    print(df[['id', 'combined_cluster']].head())
    print("\nFertig! Für Visualisierung: 'streamlit run combined_experiment.py -- --dashboard'")

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--extract", action="store_true", help="Extrahiere Features und speichere sie.")
    parser.add_argument("--cluster", action="store_true", help="Führe Clustering durch und speichere Ergebnisse.")
    parser.add_argument("--dashboard", action="store_true", help="Starte das Streamlit Dashboard.")
    parser.add_argument("--all", action="store_true", help="Führe alle Schritte automatisch aus (Default).")
    args = parser.parse_args()

    if args.extract:
        extract_and_save_features()
    elif args.cluster:
        run_clustering_and_save()
    elif args.dashboard:
        import streamlit as st
        run_dashboard()
    else:
        # Default: alles automatisch
        run_all_steps()

if __name__ == "__main__":
    main()