import csv
import json
import os
import math
import re
import time
import random
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import google.generativeai as genai

# ──────────────────────────── KONFIGURATION ────────────────────────────
load_dotenv()

LLM_MODEL = "gemini-2.5-pro"
GOOGLE_API_KEY_ENV_VAR = "GOOGLE_API_KEY"
PROMPT_FILE = os.path.join(os.path.dirname(__file__), "en_prompt.json")
MAX_WORKERS = 10
MAX_SUBMISSIONS_PER_PROMPT = 10

RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"splittability_eval_random_gemini_2_5_pro.csv")
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'prototype', 'clustering_results', 'ghc9_4_8_clustering_results.csv')

def generate_prompt(cluster, submissions):
    """Erstellt den Prompt für Gemini"""
    with open(PROMPT_FILE, 'r', encoding="utf-8") as file:
        prompt_data = json.load(file)

    role_info            = prompt_data["role"]
    instruction          = prompt_data["instruction"]
    output_format        = prompt_data["output_format"]
    input_structure_note = prompt_data["input_structure_note"]
    cluster_label_tag    = prompt_data["cluster_label"]
    submissions_label    = prompt_data["submissions_label"]
    temperature          = prompt_data.get("temperature", 0.0)

    submissions_string = ""
    for i, (source_code, error_message) in enumerate(submissions, start=1):
        submissions_string += f"""
--- Submission {i} ---
Source Code:
{source_code}

Error Message:
{error_message}
""".rstrip() + "\n"

    prompt = f"""
{role_info}
{instruction}
{input_structure_note}

{cluster_label_tag}: {cluster}

{submissions_label}:
{submissions_string}

{output_format}
""".strip()

    return temperature, prompt

def load_random_groups_from_csv():
    """
    Erstellt zufällige Submission-Gruppen. Die Anzahl und Größe der Gruppen entspricht
    exakt der Verteilung in `eval_splittability.py`, aber die Submissions werden
    zufällig aus dem *gesamten* Datensatz gezogen.
    """
    # Alle Submissions und Clustergrößen laden
    all_subs = []
    clusters = {}
    with open(CSV_FILE_PATH, encoding="utf-8") as infile:
        reader = csv.DictReader(infile)
        for row in reader:
            sid = row['submissionID']
            src = row['source_code']
            err = row['compiler_output']
            label = row['detected_cluster']
            all_subs.append((sid, src, err))
            clusters.setdefault(label, []).append((sid, src, err))

    # ▸ Clusterverteilung planen (identisch zu eval_splittability.py)
    cluster_plan = []
    for label, subs in clusters.items():
        count = len(subs)
        if count > 2:
            if count >= 100:
                groups, per_group = 10, MAX_SUBMISSIONS_PER_PROMPT
            elif count >= 30:
                groups, per_group = 3, MAX_SUBMISSIONS_PER_PROMPT
            else:
                groups  = math.ceil(count / MAX_SUBMISSIONS_PER_PROMPT)
                per_group = min(MAX_SUBMISSIONS_PER_PROMPT, count)
            cluster_plan.extend([(label, per_group)] * groups)

    # ▸ Für jede geplante Gruppe zufällige Submissions aus ALLEN auswählen
    grouped = []
    # Zähler für eindeutige Gruppennamen pro Original-Cluster
    group_counters = {}
    for original_cluster_label, group_size in cluster_plan:
        chosen = random.sample(all_subs, min(group_size, len(all_subs)))

        # Eindeutigen Namen für die Zufallsgruppe generieren
        group_counters.setdefault(original_cluster_label, 0)
        group_counters[original_cluster_label] += 1
        random_group_name = f"random_group_from_{original_cluster_label}_{group_counters[original_cluster_label]}"

        grouped.append((random_group_name, chosen))
    return grouped

def ask_llm(model, temperature_value, prompt):
    """Sendet Prompt an Gemini und gibt rohen Text zurück"""
    response = model.generate_content(
        prompt,
        generation_config=genai.types.GenerationConfig(temperature=temperature_value)
    )
    if response.prompt_feedback and response.prompt_feedback.block_reason:
        raise RuntimeError(f"Prompt blocked: {response.prompt_feedback.block_reason}")
    return response.text.strip()

RATING_RE = re.compile(r"\[Rating]\s*([1-5])\s*\[/Rating]", re.IGNORECASE | re.DOTALL)
REASON_RE = re.compile(r"\[Reason]\s*(.*?)\s*\[/Reason]", re.IGNORECASE | re.DOTALL)

def parse_response(text):
    """Extrahiert Rating (int) und Reason (str)."""
    rating_match = RATING_RE.search(text)
    reason_match = REASON_RE.search(text)
    if not rating_match or not reason_match:
        missing_parts = []
        if not rating_match:
            missing_parts.append("[Rating]")
        if not reason_match:
            missing_parts.append("[Reason]")
        raise ValueError(f"Tags nicht gefunden: {', '.join(missing_parts)}")
    return int(rating_match.group(1)), reason_match.group(1).strip()

def remove_ground_truth_metadata(src):
    # Entfernt alle Haskell-Kommentare (Block und Zeile)
    src = re.sub(r'\{\-.*?\-\}', '', src, flags=re.DOTALL)
    src = re.sub(r'--.*', '', src)
    return src

def main():
    api_key = os.getenv(GOOGLE_API_KEY_ENV_VAR)
    if not api_key:
        raise ValueError(f"Umgebungsvariable '{GOOGLE_API_KEY_ENV_VAR}' nicht gesetzt.")
    genai.configure(api_key=api_key)
    model = genai.GenerativeModel(LLM_MODEL)
    print(f"[INFO] Verwende LLM-Modell: {LLM_MODEL}")

    grouped = load_random_groups_from_csv()
    if not grouped:
        print("[WARN] Keine Submissions zum Bewerten gefunden.")
        return
    print(f"[INFO] {len(grouped)} Random-Gruppen werden bewertet …")

    def worker(cluster_label, subs):
        """Fragt das LLM ab und bereitet das Ergebnis für die CSV vor."""
        submissions_for_prompt = [(remove_ground_truth_metadata(src), err) for (_, src, err) in subs]
        sub_ids = [sid for (sid, _, _) in subs]
        rating, reason = None, None
        try:
            temperature, prompt = generate_prompt(cluster_label, submissions_for_prompt)
            raw_response = ask_llm(model, temperature, prompt)
            try:
                rating, reason = parse_response(raw_response)
            except ValueError as e:
                print(f"[WARN] Parsing für Gruppe '{cluster_label}' fehlgeschlagen: {e}")
                reason = f"PARSING_ERROR: {e}\n\nRAW_RESPONSE:\n{raw_response}"
        except Exception as e:
            print(f"[ERROR] Kritischer Fehler bei Gruppe '{cluster_label}': {e}")
            prompt = "PROMPT_GENERATION_FAILED"
            raw_response = f"CRITICAL_ERROR: {e}"
        return {
            "cluster_label": cluster_label,
            "submission_ids": json.dumps(sub_ids),
            "prompt": prompt,
            "raw_response": raw_response,
            "rating": rating,
            "reason": reason,
        }

    results_to_save = []
    start_time = time.time()

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [executor.submit(worker, cl, subs) for (cl, subs) in grouped]
        for i, fut in enumerate(as_completed(futures), start=1):
            result_dict = fut.result()
            results_to_save.append(result_dict)
            status = "OK" if result_dict["rating"] is not None else "PARSE_FAIL"
            print(f"[INFO] Bewertung {i}/{len(grouped)} abgeschlossen (Gruppe „{result_dict['cluster_label']}“, Status: {status}).")

    duration = time.time() - start_time
    print(f"[INFO] LLM-Bewertung abgeschlossen in {duration:.1f}s – speichere Ergebnisse …")

    # Ergebnisse in CSV persistieren
    with open(OUT_PATH, "w", newline='', encoding="utf-8") as outfile:
        writer = csv.DictWriter(outfile, fieldnames=[
            "cluster_label", "submission_ids", "prompt", "raw_response", "rating", "reason"
        ])
        writer.writeheader()
        for res in results_to_save:
            writer.writerow(res)

    print(f"[INFO] {len(results_to_save)} Random-Gruppenbewertungen gespeichert (Datei: {OUT_PATH}).")

if __name__ == "__main__":
    print("─── STARTE RANDOM SPLITTABILITY-EVALUATION ───")
    try:
        main()
    finally:
        print("─── RANDOM-EVALUATION ABGESCHLOSSEN ───")
