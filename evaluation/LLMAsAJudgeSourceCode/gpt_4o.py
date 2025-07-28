import os
import re
import json
import csv
import time
import random
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
from openai import OpenAI, RateLimitError

load_dotenv()
LLM_MODEL = "gpt-4o-2024-11-20"
API_KEY = os.getenv("OPENAI_API_KEY")
PROMPT_FILE = os.path.join(os.path.dirname(__file__), "en_prompt.json")
MAX_WORKERS = 3
MAX_SUBMISSIONS = 250
RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"llm_as_judge_gpt_4o.csv")
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'prototype', 'clustering_results', 'ghc9_4_8_clustering_results.csv')

client = OpenAI(api_key=API_KEY)

def generate_prompt(src, err, label):
    with open(PROMPT_FILE, "r", encoding="utf-8") as f:
        p = json.load(f)
    prompt = f"""
{p['role']}
{p['instruction']}

{p['source_code_label']}:
{src}

{p['cluster_label']}: {label}

{p['output_format']}
"""
    temperature = p.get("temperature", 0.0)
    return prompt.strip(), temperature

def ask_llm(model_name, temperature, prompt, backoff=2):
    attempt = 0
    while True:
        try:
            response = client.chat.completions.create(
                model=model_name,
                temperature=temperature,
                messages=[{"role": "user", "content": prompt}]
            )
            return response.choices[0].message.content.strip()
        except RateLimitError:
            wait = backoff * (2 ** attempt) + random.uniform(0, backoff)
            print(f"[RETRY] Rate‑Limit (429) – Versuch {attempt + 1}. Warte {wait:.2f}s …")
            time.sleep(wait)
            attempt += 1
        except Exception as e:
            raise e

RATING_RE  = re.compile(r"\[Rating]\s*([1-5])\s*\[/Rating]", re.I | re.S)
REASON_RE  = re.compile(r"\[Reason]\s*(.*?)\s*\[/Reason]", re.I | re.S)

def parse_response(text):
    rating_match = RATING_RE.search(text)
    reason_match = REASON_RE.search(text)
    if rating_match:
        rating = int(rating_match.group(1))
        if reason_match:
            reason = reason_match.group(1).strip()
        else:
            idx = text.lower().find("[reason]")
            reason = text[idx + 8:].strip() if idx != -1 else text.strip()
    else:
        rating = 0
        reason = text.strip()
    return rating, reason

def remove_ground_truth_metadata(src):
    src = re.sub(r'\{\-.*?\-\}', '', src, flags=re.DOTALL)
    src = re.sub(r'--.*', '', src)
    return src

def main():
    print(f"[INFO] Verwende OpenAI-Modell: {LLM_MODEL}")
    with open(CSV_FILE_PATH, encoding="utf-8") as infile, open(OUT_PATH, "w", newline='', encoding="utf-8") as outfile:
        reader = csv.DictReader(infile)
        writer = csv.DictWriter(outfile, fieldnames=['submissionID', 'cluster_label', 'rating', 'reason', 'source_code', 'error_message', 'full_prompt'])
        writer.writeheader()
        rows = [
            (row['submissionID'], row['detected_cluster'], row['source_code'], row['compiler_output'])
            for row in reader
        ][:MAX_SUBMISSIONS]
        total = len(rows)
        def worker(row):
            sid, label, src, err = row
            src_clean = remove_ground_truth_metadata(src)
            prompt, temperature = generate_prompt(src_clean, err, label)
            raw_response = ask_llm(LLM_MODEL, temperature, prompt)
            rating, reason = parse_response(raw_response)
            return {
                'submissionID': sid,
                'cluster_label': label,
                'rating': rating,
                'reason': reason,
                'source_code': src_clean,
                'error_message': err,
                'full_prompt': prompt
            }
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            futures = [executor.submit(worker, r) for r in rows]
            for idx, fut in enumerate(as_completed(futures), 1):
                try:
                    result = fut.result()
                    writer.writerow(result)
                    print(f"[{idx}/{total}] bewertet: submissionID {result['submissionID']}")
                except Exception as e:
                    print(f"[WARN] Bewertung übersprungen: {e}")

if __name__ == "__main__":
    print("─── STARTE CLUSTER-LABEL-EVALUATION ───")
    try:
        main()
    finally:
        print("─── EVALUATION ABGESCHLOSSEN ───")
