import streamlit as st
import pandas as pd
import numpy as np
import re
from pathlib import Path
import os
import sys

from tree_sitter import Language, Parser
import networkx as nx

from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
import plotly.express as px

# Configuration
CONFIG = {
    "DATA_SOURCE": {
        "CSV_PATH": "../../../compilation/compilation_results/ghc9_4_8_results.csv",
        "VL_NR": None,
        "ERROR_SOURCE": None
    },
    "CLUSTERING": {
        "NUM_CLUSTERS": 8
    }
}

if 'streamlit' in sys.modules:
    st.set_page_config(page_title="AST-Only Clustering Dashboard", layout="wide")
    st.title("Clustering Based on AST Features Only")

# Data loading
csv_path = CONFIG["DATA_SOURCE"]["CSV_PATH"]
vl_nr = CONFIG["DATA_SOURCE"]["VL_NR"]
error_source = CONFIG["DATA_SOURCE"]["ERROR_SOURCE"]

if not os.path.exists(csv_path):
    if 'streamlit' in sys.modules:
        st.error(f"CSV file not found: {csv_path}")
        st.stop()
    else:
        print(f"❌ CSV file not found: {csv_path}")
        sys.exit(1)

df = pd.read_csv(csv_path)
if 'id' not in df.columns or 'source_code' not in df.columns:
    if 'streamlit' in sys.modules:
        st.error("CSV must contain columns 'id' and 'source_code'.")
        st.stop()
    else:
        print("❌ CSV must contain columns 'id' and 'source_code'.")
        sys.exit(1)

if vl_nr is not None:
    df = df[df['vl_nr'] == vl_nr]
if error_source is not None:
    df = df[df['error_source'] == error_source]

if df.empty:
    if 'streamlit' in sys.modules:
        st.warning("No data found for the selected filters.")
        st.stop()
    else:
        print("⚠️ No data found for the selected filters.")
        sys.exit(1)

# Tree-sitter setup
def setup_tree_sitter_internal():
    """Internal function that does the actual work"""
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
        error_msg = "tree-sitter-haskell directory not found. Please run: git submodule update --init --recursive"
        print(f"❌ {error_msg}")
        return None

    PROJECT_ROOT = haskell_grammar_path.parent
    BUILD_DIR = PROJECT_ROOT / "build"
    BUILD_DIR.mkdir(exist_ok=True)
    
    possible_so_files = [
        BUILD_DIR / "my-languages.so",
        BUILD_DIR / "haskell.so", 
        haskell_grammar_path / "haskell.so",
        PROJECT_ROOT / "haskell.so"
    ]
    
    LANG_SO = None
    for so_file in possible_so_files:
        if so_file.exists():
            LANG_SO = so_file
            print(f"✅ Found existing library: {LANG_SO}")
            break
    
    if LANG_SO is None:
        LANG_SO = BUILD_DIR / "my-languages.so"
        print("🔨 Building tree-sitter Haskell parser...")
        try:
            Language.build_library(str(LANG_SO), [str(haskell_grammar_path)])
            print("✅ Tree-sitter Haskell parser built successfully!")
        except AttributeError:
            print("❌ Current py-tree-sitter version doesn't support build_library")
            print("💡 Please install compatible version: pip install tree-sitter==0.20.4")
            return None
        except Exception as e:
            print(f"❌ Failed to build tree-sitter library: {e}")
            return None

    try:
        HASKELL_LANGUAGE = Language(str(LANG_SO), "haskell")
        parser = Parser()
        parser.set_language(HASKELL_LANGUAGE)
        return parser
    except TypeError:
        try:
            HASKELL_LANGUAGE = Language(str(LANG_SO))
            parser = Parser()
            parser.set_language(HASKELL_LANGUAGE)
            return parser
        except Exception as e:
            print(f"❌ Failed to load Haskell language: {e}")
            return None
    except Exception as e:
        print(f"❌ Failed to load Haskell language: {e}")
        return None

def get_parser():
    """Get parser without Streamlit caching"""
    return setup_tree_sitter_internal()

@st.cache_resource
def setup_tree_sitter():
    """Streamlit cached version"""
    parser = setup_tree_sitter_internal()
    if parser is None:
        st.error("""
        Failed to setup tree-sitter. Please try one of these solutions:
        
        1. Install tree-sitter CLI: `npm install -g tree-sitter-cli`
        2. Check submodule: `git submodule update --init --recursive`
        3. Manual build: `cd tree-sitter-haskell && tree-sitter generate && tree-sitter build --output ../build/haskell.so`
        """)
        st.stop()
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

def main_streamlit():
    parser = setup_tree_sitter()
    st.info("Extracting AST features. This may take a few minutes for many submissions...")

    features = []
    for i, row in df.iterrows():
        code = row['source_code']
        clean_code = clean_haskell_code_for_embedding(code)
        g = build_graph_from_code(clean_code, parser)
        v = graph_to_feature_vector(g)
        features.append(v)
    X1 = np.vstack(features)

    num_clusters = st.sidebar.slider("Number of clusters for KMeans", 2, 30, CONFIG["CLUSTERING"]["NUM_CLUSTERS"])
    kmeans = KMeans(n_clusters=num_clusters, n_init=10, random_state=0)
    labels = kmeans.fit_predict(X1)
    df["ast_cluster"] = labels

    if len(df) > 2:
        perplexity = max(5, min(30, len(df) // 3))
        tsne = TSNE(n_components=2, perplexity=perplexity, metric="cosine", random_state=0)
        coords = tsne.fit_transform(X1)
        df["x"], df["y"] = coords[:, 0], coords[:, 1]
    else:
        df["x"] = df["y"] = 0.0

    st.sidebar.header("Cluster Filter")
    cluster_values = sorted(df["ast_cluster"].unique())
    cluster_options = ["All"] + cluster_values
    cluster_choice = st.sidebar.selectbox("Select Cluster", cluster_options)

    df_view = df if cluster_choice == "All" else df[df["ast_cluster"] == cluster_choice]

    st.markdown("### t-SNE Projection (AST Features)")
    if "x" in df.columns:
        fig = px.scatter(
            df_view, x="x", y="y", color=df_view["ast_cluster"].astype(str),
            hover_data=["id"], title=f"Clustering Based on AST Features – Cluster: {cluster_choice}",
            height=650
        )
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.info("Less than three submissions – no projection.")

    st.markdown("### Cluster Sizes (AST-based)")
    size_series = df["ast_cluster"].value_counts().sort_index()
    st.bar_chart(size_series)

    st.markdown(f"### Code Examples – Cluster {cluster_choice}")
    if len(df_view) == 0:
        st.info("No submissions in this cluster.")
    else:
        sample_df = df_view.sample(min(10, len(df_view)), random_state=1)
        for _, row in sample_df.iterrows():
            with st.expander(f"Submission {row['id']}"):
                st.code(row.get("source_code", "[Code not available]"), language="haskell")

def main_cli():
    print("🚀 Running AST clustering in CLI mode...")
    
    parser = get_parser()
    if parser is None:
        sys.exit("❌ Failed to setup tree-sitter parser")
    
    print("✅ Parser setup successful")
    print(f"📊 Processing {len(df)} submissions...")
    
    features = []
    for i, row in df.iterrows():
        if i % 10 == 0:
            print(f"  Processing {i+1}/{len(df)}...")
        code = row['source_code']
        clean_code = clean_haskell_code_for_embedding(code)
        g = build_graph_from_code(clean_code, parser)
        v = graph_to_feature_vector(g)
        features.append(v)
    
    X1 = np.vstack(features)
    print(f"✅ Feature extraction complete. Shape: {X1.shape}")
    
    num_clusters = CONFIG["CLUSTERING"]["NUM_CLUSTERS"]
    kmeans = KMeans(n_clusters=num_clusters, n_init=10, random_state=0)
    labels = kmeans.fit_predict(X1)
    
    print(f"✅ Clustering complete with {num_clusters} clusters")
    print(f"📈 Cluster distribution: {np.bincount(labels)}")

if __name__ == "__main__":
    try:
        main_streamlit()
    except:
        print("💡 Running in CLI mode. Use 'streamlit run ast_experiment.py' for the web interface.")
        main_cli()
else:
    main_streamlit()

# =============================================================================
# 5. MAIN ENTRY POINT
# =============================================================================
def main_cli():
    """Command line version for testing"""
    print("🚀 Running AST clustering in CLI mode...")
    
    parser = get_parser()
    if parser is None:
        sys.exit("❌ Failed to setup tree-sitter parser")
    
    print("✅ Parser setup successful")
    print(f"📊 Processing {len(df)} submissions...")
    
    features = []
    for i, row in df.iterrows():
        if i % 10 == 0:
            print(f"  Processing {i+1}/{len(df)}...")
        code = row['source_code']
        clean_code = clean_haskell_code_for_embedding(code)
        g = build_graph_from_code(clean_code, parser)
        v = graph_to_feature_vector(g)
        features.append(v)
    
    X1 = np.vstack(features)
    print(f"✅ Feature extraction complete. Shape: {X1.shape}")
    
    # Simple clustering
    num_clusters = CONFIG["CLUSTERING"]["NUM_CLUSTERS"]
    kmeans = KMeans(n_clusters=num_clusters, n_init=10, random_state=0)
    labels = kmeans.fit_predict(X1)
    
    print(f"✅ Clustering complete with {num_clusters} clusters")
    print(f"📈 Cluster distribution: {np.bincount(labels)}")

if __name__ == "__main__":
    # Check if running in Streamlit context
    try:
        main_streamlit()
    except:
        print("💡 Running in CLI mode. Use 'streamlit run ast_experiment.py' for the web interface.")
        main_cli()
else:
    # Running in Streamlit
    main_streamlit()
