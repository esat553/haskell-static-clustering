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
import google.generativeai as genai
from google.api_core.exceptions import ResourceExhausted, InternalServerError

load_dotenv()

KONFIG = {
    "DATENQUELLE": {
        "CSV_PFAD": "../../../compilation/compilation_results/ghc9_4_8_results.csv",
        "VL_NR": None,
        "AUFGABEN_ID": None
    },
    "GEMINI_ANALYSE": {
        "MODELL_ID": "gemini-2.5-flash",
        "BATCH_SIZE": 8,
        "SCHLAF_SEKUNDEN": 1.5,
        "PROMPT": """
You are an experienced Haskell programmer and tutor. Analyze the following source code and the corresponding compiler error message.
Your task is to formulate the actual root cause of the problem in a short, concise sentence.
Respond ONLY with this one sentence. Do not provide any introduction, code improvements, or any other explanations.

Example 1:
- Code: `let x = 5 in x + "a"`
- Error message: `No instance for (Num [Char]) arising from a use of '+'`
- Your answer: The plus operator is incorrectly applied to a number and a string.

Example 2:
- Code: `head []`
- Error message: `*** Exception: Prelude.head: empty list`
- Your answer: The function 'head' is applied to an empty list, which is not defined.

Here is the data to analyze:

[HASKELL CODE]
{code}

[COMPILER ERROR]
{error}

ROOT CAUSE:
"""
    },
    "GEMINI_EMBEDDING": {
        "MODELL_ID": "gemini-embedding-exp-03-07",
        "BATCH_SIZE": 16,
        "SCHLAF_SEKUNDEN": 1.1,
        "TASK_TYPE": "CLUSTERING"
    },
    "UMAP": {
        "n_neighbors": 10,
        "min_dist": 0.0,
        "metric": "cosine",
        "random_state": 422
    },
    "HDBSCAN": {
        "min_cluster_size": 3,
        "min_samples": 1,
        "metric": "euclidean",
        "cluster_selection_epsilon": 0.5
    },
    "DATEIEN": {
        "final_clustered_data": "clustered_data_with_analysis.csv",
        "embeddings_npy": "embeddings_analysis.npy",
        "umap_2d": "umap_2d_analysis.npy",
        "umap_model": "umap_model_analysis.pkl",
        "prepared_data": "prepared_for_analysis.csv"
    }
}

def bereinige_code_und_fehler(code, fehler):
    sauberer_code = re.sub(r'--.*', '', code)
    sauberer_code = re.sub(r'{-(.|\n)*?-}', '', sauberer_code, flags=re.DOTALL).strip()
    sauberer_fehler = re.sub(r'\s*\n\s*', ' ', fehler).strip()
    return f'[HASKELL CODE]\n{sauberer_code}\n\n[COMPILER ERROR]\n{sauberer_fehler}'

def lade_und_bereite_daten(konfig):
    pfad = konfig['DATENQUELLE']['CSV_PFAD']
    vl_nr = konfig['DATENQUELLE'].get('VL_NR')
    aufgaben_id = konfig['DATENQUELLE'].get('AUFGABEN_ID')
    df = pd.read_csv(pfad)
    if 'id' not in df.columns or 'source_code' not in df.columns or 'compiler_output' not in df.columns:
        raise ValueError("CSV muss die Spalten 'id', 'source_code' und 'compiler_output' enthalten.")
    if vl_nr is not None:
        df = df[df['vl_nr'] == vl_nr]
    if aufgaben_id is not None:
        df = df[df['taskID'] == aufgaben_id]
    df = df.drop_duplicates(subset=['source_code', 'compiler_output']).reset_index(drop=True)
    df = df.rename(columns={'id': 'submissionID'})
    df[['submissionID', 'source_code', 'compiler_output']].to_csv(konfig['DATEIEN']['prepared_data'], index=False)
    return df

def analysiere_root_causes(df, konfig):
    analyse_cfg = konfig['GEMINI_ANALYSE']
    mein_api_key = os.getenv("GOOGLE_API_KEY")
    genai.configure(api_key=mein_api_key)
    load_dotenv()
    if not (api_key := os.getenv("GOOGLE_API_KEY")):
        raise RuntimeError("GOOGLE_API_KEY fehlt!")
    modell = genai.GenerativeModel(analyse_cfg['MODELL_ID'])
    @backoff.on_exception(backoff.expo, (ResourceExhausted, InternalServerError), max_time=600)
    def analysiere_eintrag(code, fehler):
        prompt = analyse_cfg['PROMPT'].format(code=code, error=fehler)
        antwort = modell.generate_content(prompt)
        return antwort.text.strip()
    analysen = []
    for _, zeile in tqdm(df.iterrows(), total=df.shape[0], desc="Fehlerursachen analysieren"):
        try:
            analyse = analysiere_eintrag(zeile['source_code'], zeile['compiler_output'])
            analysen.append(analyse)
        except Exception as e:
            print(f"FEHLER bei Analyse für ID {zeile['submissionID']}: {e}. Fülle mit Standardtext.")
            analysen.append("Fehler bei der Analyse.")
        time.sleep(analyse_cfg['SCHLAF_SEKUNDEN'])
    df['root_cause_analysis'] = analysen
    return df

def generiere_embeddings(df, konfig):
    embedding_cfg, dateien_cfg = konfig['GEMINI_EMBEDDING'], konfig['DATEIEN']
    load_dotenv()
    mein_api_key = os.getenv("GOOGLE_API_KEY")
    genai.configure(api_key=mein_api_key)
    if not (api_key := os.getenv("GOOGLE_API_KEY")):
        raise RuntimeError("GOOGLE_API_KEY fehlt!")
    texte = df["root_cause_analysis"].tolist()
    @backoff.on_exception(backoff.expo, (ResourceExhausted, InternalServerError), max_time=600)
    def embed_batch(chunk):
        return genai.embed_content(model=embedding_cfg['MODELL_ID'], content=chunk, task_type=embedding_cfg['TASK_TYPE'])
    alle_embeddings = []
    for i in tqdm(range(0, len(texte), embedding_cfg['BATCH_SIZE']), desc="Embedding Batches"):
        batch = texte[i:i + embedding_cfg['BATCH_SIZE']]
        try:
            result = embed_batch(batch)
            alle_embeddings.extend(result['embedding'])
        except Exception as e:
            print(f"FEHLER bei Batch {i}: {e}. Fülle mit Nullen auf.")
            alle_embeddings.extend([[0.0] * 768] * len(batch))
        time.sleep(embedding_cfg['SCHLAF_SEKUNDEN'])
    vec_array = np.asarray(alle_embeddings, dtype=np.float32)
    np.save(dateien_cfg['embeddings_npy'], vec_array)
    return vec_array

def reduziere_dimension_umap(embeddings, konfig):
    dateien_cfg, umap_params = konfig['DATEIEN'], konfig['UMAP'].copy()
    n_samples = embeddings.shape[0]
    if n_samples <= umap_params['n_neighbors']:
        umap_params['n_neighbors'] = max(1, n_samples - 1)
    umap_modell = umap.UMAP(**umap_params)
    umap_2d = umap_modell.fit_transform(embeddings)
    np.save(dateien_cfg['umap_2d'], umap_2d)
    joblib.dump(umap_modell, dateien_cfg['umap_model'])
    return umap_2d

def fuehre_clustering_hdbscan_durch(umap_2d, konfig):
    hdbscan_params = konfig['HDBSCAN'].copy()
    n_samples = umap_2d.shape[0]
    if n_samples < hdbscan_params['min_cluster_size']:
        neue_groesse = max(2, n_samples // 2 if n_samples > 3 else 2)
        hdbscan_params['min_cluster_size'] = neue_groesse
    clusterer = hdbscan.HDBSCAN(**hdbscan_params)
    labels = clusterer.fit_predict(umap_2d)
    return labels

def dashboard_ausfuehren(konfig):
    st.set_page_config(layout="wide")
    st.title("Interaktive Cluster-Analyse von Haskell-Fehlern (Root-Cause-basiert)")
    WRAP_AT = 100
    try:
        df = pd.read_csv(konfig['DATEIEN']['final_clustered_data'])
    except FileNotFoundError:
        st.error("Benötigte Datendatei nicht gefunden. Bitte zuerst die Pipeline ausführen.")
        st.stop()
    df['anzeige_text'] = df.apply(
        lambda zeile: bereinige_code_und_fehler(str(zeile['source_code']), str(zeile['compiler_output'])),
        axis=1
    )
    df['hover_text'] = df['anzeige_text'].apply(
        lambda txt: "<br>".join(textwrap.wrap(txt.replace('\n', ' '), WRAP_AT, replace_whitespace=False))
    )
    st.info(f"Analyse für **CSV: {konfig['DATENQUELLE']['CSV_PFAD']}** | Einträge: **{len(df)}**")
    st.sidebar.header("Filter & Optionen")
    cluster_ids = sorted(df["cluster_id"].unique())
    options_list = ["Alle"] + sorted([cid for cid in cluster_ids if cid != -1])
    if -1 in cluster_ids:
        options_list.append(-1)
    ausgewaehlter_cluster = st.sidebar.selectbox(
        "Cluster auswählen",
        options=options_list,
        format_func=lambda x: "Rauschen (-1)" if x == -1 else f"Cluster {x}"
    )
    gefiltertes_df = df if ausgewaehlter_cluster == "Alle" else df[df["cluster_id"] == ausgewaehlter_cluster]
    header_text = "Übersicht aller Cluster"
    if ausgewaehlter_cluster != "Alle":
        header_text = f"Details für Cluster {ausgewaehlter_cluster}" if ausgewaehlter_cluster != -1 else "Details für Rauschpunkte"
    st.header(header_text)
    fig = px.scatter(
        gefiltertes_df,
        x="umap_1", y="umap_2",
        color="cluster_id",
        hover_name="submissionID",
        hover_data={
            "cluster_id": True,
            "root_cause_analysis": True,
            "hover_text": True,
            "umap_1": False, "umap_2": False,
        },
        title=f"UMAP + HDBSCAN Clustering (Cluster: {ausgewaehlter_cluster})"
    )
    fig.update_layout(hoverlabel=dict(align="left"))
    st.plotly_chart(fig, use_container_width=True)
    if not gefiltertes_df.empty:
        st.markdown("### Repräsentative Einträge des Clusters")
        beispiel_anzahl = min(5, len(gefiltertes_df))
        for _, zeile in gefiltertes_df.sample(beispiel_anzahl, random_state=42).iterrows():
            with st.expander(f"Submission ID: {zeile['submissionID']} | Analyse: {zeile['root_cause_analysis']}"):
                st.code(zeile['anzeige_text'], language='text')

def main():
    ausgabe_ordner = KONFIG['DATEIEN'].get('OUTPUT_DIR')
    if ausgabe_ordner:
        os.makedirs(ausgabe_ordner, exist_ok=True)
    if not os.path.exists(KONFIG['DATEIEN']['final_clustered_data']):
        st.info("Initialisiere Analyse-Pipeline... Dies kann einige Minuten dauern.")
        with st.spinner("Schritt 1/5: Lade und bereite Daten vor..."):
            df = lade_und_bereite_daten(KONFIG)
        with st.spinner("Schritt 2/5: Analysiere Fehlerursachen (API-Calls zu Gemini)..."):
            df = analysiere_root_causes(df, KONFIG)
        with st.spinner("Schritt 3/5: Generiere Embeddings aus Analysen (API-Calls)..."):
            embeddings = generiere_embeddings(df, KONFIG)
        with st.spinner("Schritt 4/5: Reduziere Dimensionen mit UMAP..."):
            umap_2d = reduziere_dimension_umap(embeddings, KONFIG)
            df["umap_1"], df["umap_2"] = umap_2d[:, 0], umap_2d[:, 1]
        with st.spinner("Schritt 5/5: Führe Clustering mit HDBSCAN durch..."):
            cluster_labels = fuehre_clustering_hdbscan_durch(umap_2d, KONFIG)
            df["cluster_id"] = cluster_labels
        df.to_csv(KONFIG['DATEIEN']['final_clustered_data'], index=False)
        st.success("Pipeline erfolgreich abgeschlossen! Lade Dashboard...")
        st.rerun()
    else:
        dashboard_ausfuehren(KONFIG)

if __name__ == "__main__":
    main()