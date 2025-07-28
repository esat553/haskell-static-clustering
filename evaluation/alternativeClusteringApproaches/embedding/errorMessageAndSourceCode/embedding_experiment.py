# haskell_cluster_pipeline.py (REFACTORED VERSION - Combined Source Code & Error Message Embedding)

import os
import re
import time
import joblib
import backoff
import numpy as np
import pandas as pd
import umap
import hdbscan
import streamlit as st
import plotly.express as px
import textwrap
from tqdm import tqdm
from dotenv import load_dotenv
from google import genai
from google.genai import types
from google.api_core.exceptions import ResourceExhausted, InternalServerError

# =============================================================================
# 1. CONFIGURATION
# =============================================================================
CONFIG = {
    "DATA_SOURCE": {
        "CSV_PATH": "../../../../compilation/compilation_results/ghc9_4_8_results.csv",
        "VL_NR": None,
        "ERROR_SOURCE": None
    },
    "GEMINI": {
        "MODEL_ID": "gemini-embedding-001", "BATCH_SIZE": 16,
        "SLEEP_SECONDS": 1.1, "TASK_TYPE": "CLUSTERING"
    },
    "UMAP": {
        "n_neighbors": 15, "min_dist": 0.1, "metric": "cosine", "random_state": 42
    },
    "HDBSCAN": {
        "min_cluster_size": 2, "metric": "euclidean"
    },
    "FILES": {
        "OUTPUT_DIR": "embedding_results",
        "prepared_data": "embedding_results/prepared_for_embedding_combined.csv",
        "embeddings_npy": "embedding_results/embeddings_combined.npy",
        "embeddings_df": "embedding_results/embeddings_combined.parquet",
        "umap_2d": "embedding_results/umap_2d_combined.npy",
        "umap_model": "embedding_results/umap_model_combined.pkl",
        "cluster_labels": "embedding_results/cluster_labels_combined.npy",
        "final_clustered_data": "embedding_results/clustered_data_combined.csv"
    }
}

# =============================================================================
# 2. DATA PROCESSING & ML PIPELINE
# =============================================================================

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

def get_clean_code_and_error(code, error):
    clean_code = re.sub(r'--.*', '', str(code))
    clean_code = re.sub(r'{-(.|\n)*?-}', '', clean_code, flags=re.DOTALL).strip()
    error_blocks = re.split(r'\n(?=.*?:\d+:\d+: error:)', str(error))
    processed_errors = [msg for block in error_blocks if block.strip() and (msg := process_single_error_block(block))]
    final_error_message = ". ".join(processed_errors)
    if not final_error_message:
        final_error_message = re.sub(r'\s*\n\s*', ' ', str(error)).strip()
    return f'[HASKELL CODE]\n{clean_code}\n[COMPILER ERROR]\n{final_error_message.strip()}'

def load_and_prepare_data(cfg):
    files_cfg = cfg['FILES']
    csv_path = cfg['DATA_SOURCE']['CSV_PATH']
    vl_nr = cfg['DATA_SOURCE'].get('VL_NR', None)
    error_source = cfg['DATA_SOURCE'].get('ERROR_SOURCE', None)

    print(f"Loading data from CSV: {csv_path}")
    df = pd.read_csv(csv_path)
    if 'id' not in df.columns or 'source_code' not in df.columns or 'compiler_output' not in df.columns:
        raise ValueError("CSV must contain 'id', 'source_code', and 'compiler_output' columns.")

    if vl_nr is not None:
        df = df[df['vl_nr'] == vl_nr]
    if error_source is not None:
        df = df[df['error_source'] == error_source]

    if df.empty:
        st.warning("No data found for the selected filters.")
        st.stop()

    df['combined_text'] = df.apply(lambda r: get_clean_code_and_error(r['source_code'], r['compiler_output']), axis=1)
    df.rename(columns={'id': 'submissionID'})[['submissionID', 'combined_text']].to_csv(files_cfg['prepared_data'], index=False)
    print(f"✅ {len(df)} data points prepared and saved to '{files_cfg['prepared_data']}'.")
    return df

def generate_embeddings(cfg):
    gemini_cfg, files_cfg = cfg['GEMINI'], cfg['FILES']
    load_dotenv()
    if not (api_key := os.getenv("GOOGLE_API_KEY")):
        raise RuntimeError("GOOGLE_API_KEY not found in environment variables.")
    client = genai.Client(api_key=api_key)
    df = pd.read_csv(files_cfg['prepared_data'])
    texts = df["combined_text"].tolist()
    print(f"Generating embeddings for {len(texts)} entries...")

    @backoff.on_exception(backoff.expo, (ResourceExhausted, InternalServerError), max_time=600)
    def embed_batch(chunk):
        resp = client.models.embed_content(
            model=gemini_cfg['MODEL_ID'],
            contents=chunk,
            config=types.EmbedContentConfig(task_type=gemini_cfg['TASK_TYPE'])
        )
        return [e.values for e in resp.embeddings]

    all_embeddings = []
    for i in tqdm(range(0, len(texts), gemini_cfg['BATCH_SIZE']), desc="Embedding Batches"):
        batch = texts[i:i + gemini_cfg['BATCH_SIZE']]
        try:
            all_embeddings.extend(embed_batch(batch))
        except Exception as e:
            print(f"ERROR in batch {i}: {e}. Filling with zeros.")
            all_embeddings.extend([[0.0] * 768] * len(batch))
        time.sleep(gemini_cfg['SLEEP_SECONDS'])

    vec_array = np.asarray(all_embeddings, dtype=np.float32)
    np.save(files_cfg['embeddings_npy'], vec_array)
    df["embedding"] = [v.tolist() for v in vec_array]
    try:
        df.to_parquet(files_cfg['embeddings_df'], index=False)
    except ImportError:
        df.to_csv(files_cfg['embeddings_df'].replace(".parquet", ".csv"), index=False)
    print(f"✅ {len(all_embeddings)} embeddings saved.")
    return vec_array

def reduce_dimension_umap(cfg):
    embeddings = np.load(cfg['FILES']['embeddings_npy'])
    n_samples = embeddings.shape[0]
    umap_params = cfg['UMAP'].copy()
    if n_samples <= umap_params['n_neighbors']:
        umap_params['n_neighbors'] = max(1, n_samples - 1)
        print(f"WARNING: n_neighbors > samples. Reducing to {umap_params['n_neighbors']}.")
    umap_model = umap.UMAP(**umap_params)
    umap_2d = umap_model.fit_transform(embeddings)
    np.save(cfg['FILES']['umap_2d'], umap_2d)
    joblib.dump(umap_model, cfg['FILES']['umap_model'])
    print(f"✅ UMAP model and 2D coordinates saved.")
    return umap_2d

def perform_clustering_hdbscan(cfg):
    umap_2d = np.load(cfg['FILES']['umap_2d'])
    n_samples = umap_2d.shape[0]
    hdbscan_params = cfg['HDBSCAN'].copy()
    if n_samples < hdbscan_params['min_cluster_size']:
        new_size = max(2, n_samples // 2 if n_samples > 3 else 2)
        print(f"WARNING: min_cluster_size > samples. Reducing to {new_size}.")
        hdbscan_params['min_cluster_size'] = new_size
    clusterer = hdbscan.HDBSCAN(**hdbscan_params)
    labels = clusterer.fit_predict(umap_2d)
    df = pd.read_csv(cfg['FILES']['prepared_data'])
    df["cluster_id"] = labels
    df.to_csv(cfg['FILES']['final_clustered_data'], index=False)
    num_clusters = len(set(labels)) - (1 if -1 in labels else 0)
    print(f"✅ Clustering complete: {num_clusters} clusters found.")
    return df

# =============================================================================
# 3. STREAMLIT DASHBOARD
# =============================================================================
def run_dashboard(cfg):
    st.set_page_config(layout="wide")
    st.title("Interactive Cluster Analysis of Haskell Source Code & Error Messages")

    WRAP_AT = 80

    try:
        df = pd.read_csv(cfg['FILES']['final_clustered_data'])
        umap_2d = np.load(cfg['FILES']['umap_2d'])
    except FileNotFoundError:
        st.error("Required data files not found. Please run the pipeline first.")
        st.stop()

    df["umap_1"], df["umap_2"] = umap_2d[:, 0], umap_2d[:, 1]

    df['hover_text'] = df['combined_text'].apply(
        lambda txt: "<br>".join(textwrap.wrap(txt.replace('\n', ' <br>'), WRAP_AT, replace_whitespace=False))
    )

    st.info(f"Analysis for **CSV: {cfg['DATA_SOURCE']['CSV_PATH']}** | Entries: **{len(df)}**")

    st.sidebar.header("Filters & Options")
    cluster_ids = sorted(df["cluster_id"].unique())
    options_list = ["All"] + sorted([cid for cid in cluster_ids if cid != -1])
    if -1 in cluster_ids:
        options_list.append(-1)
    selected_cluster = st.sidebar.selectbox(
        "Select Cluster",
        options=options_list,
        format_func=lambda x: "Noise" if x == -1 else str(x)
    )

    filtered_df = df if selected_cluster == "All" else df[df["cluster_id"] == selected_cluster]

    header_text = "Overview of all Clusters"
    if selected_cluster != "All":
        header_text = f"Details for Cluster {selected_cluster}" if selected_cluster != -1 else "Details for Noise Points"
    st.header(header_text)

    fig = px.scatter(
        filtered_df,
        x="umap_1",
        y="umap_2",
        color="cluster_id",
        hover_name="submissionID",
        hover_data={
            "cluster_id": True,
            "hover_text": True,
            "umap_1": False,
            "umap_2": False,
        },
        title=f"UMAP + HDBSCAN Clustering (Cluster: {selected_cluster})"
    )

    fig.update_layout(hoverlabel=dict(align="left"))
    st.plotly_chart(fig, use_container_width=True)

    if not filtered_df.empty:
        st.markdown("### Sample Submission from Plot")
        if options := filtered_df['submissionID'].tolist():
            selected_id = st.selectbox("Select Submission ID", options)
            row = filtered_df[filtered_df['submissionID'] == selected_id].iloc[0]
            combined = row["combined_text"]
            if '[HASKELL CODE]' in combined and '[COMPILER ERROR]' in combined:
                code_part = combined.split('[HASKELL CODE]')[1].split('[COMPILER ERROR]')[0].strip()
                error_part = combined.split('[COMPILER ERROR]')[1].strip()
                st.markdown("**Source Code:**")
                st.code(code_part, language='haskell')
                st.markdown("**Compiler Error:**")
                st.code(error_part, language='text')
            else:
                st.code(combined, language='text')

# =============================================================================
# 4. MAIN ORCHESTRATION
# =============================================================================
def main():
    output_dir = CONFIG['FILES'].get('OUTPUT_DIR')
    if output_dir:
        os.makedirs(output_dir, exist_ok=True)

    if not os.path.exists(CONFIG['FILES']['final_clustered_data']):
        st.info("Initializing pipeline... This may take a few minutes.")
        steps = [
            ("Step 1/4: Loading and preparing data...", load_and_prepare_data),
            ("Step 2/4: Generating embeddings (API calls)...", generate_embeddings),
            ("Step 3/4: Reducing dimensions with UMAP...", reduce_dimension_umap),
            ("Step 4/4: Performing clustering with HDBSCAN...", perform_clustering_hdbscan)
        ]
        for msg, func in steps:
            with st.spinner(msg):
                func(CONFIG)
        st.success("Pipeline completed successfully! Loading dashboard...")
        st.rerun()
    else:
        run_dashboard(CONFIG)

if __name__ == "__main__":
    main()