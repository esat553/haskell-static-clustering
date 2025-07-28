import os
import re
import json
import time
import random
import csv
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import anthropic

load_dotenv()

LLM_MODEL = "claude-sonnet-4-20250514"
ANTHROPIC_API_KEY_ENV_VAR = "CLAUDE_API_KEY"
PROMPT_FILE = os.path.join(os.path.dirname(__file__), "en_prompt.json")
MAX_SUBMISSIONS = 250
MAX_WORKERS = 5
RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"llm_as_judge_sonnet_4.csv")
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'prototype', 'clustering_results', 'ghc9_4_8_clustering_results.csv')

client = anthropic.Anthropic(api_key=os.getenv(ANTHROPIC_API_KEY_ENV_VAR))

def generate_prompt(src: str, err: str, label: str):
    try:
        with open(PROMPT_FILE, "r", encoding="utf-8") as openfile:
            prompt_data = json.load(openfile)
    except FileNotFoundError:
        raise FileNotFoundError(f"Prompt-Datei '{PROMPT_FILE}' nicht gefunden.")
    except json.JSONDecodeError as e:
        raise ValueError(f"Fehler beim Parsen der JSON-Datei '{PROMPT_FILE}': {e}")

    role_info = prompt_data["role"]
    instruction = prompt_data["instruction"]
    output_format = prompt_data["output_format"]
    source_code_label = prompt_data["source_code_label"]
    cluster_label_text = prompt_data["cluster_label"]
    temperature = prompt_data.get("temperature", 0.0)

    prompt_string = f"""
{role_info}
{instruction}

{source_code_label}:
{src}

{cluster_label_text}: {label}

{output_format}
"""
    return prompt_string.strip(), temperature

def ask_llm(model_name: str, temperature: float, prompt: str, backoff: int = 2) -> str:
    attempt = 0
    while True:
        try:
            response = client.messages.create(
                model=model_name,
                max_tokens=2048,
                temperature=temperature,
                messages=[{"role": "user", "content": prompt}]
            )
            return response.content[0].text.strip()
        except anthropic.RateLimitError:
            wait = backoff * (2 ** attempt) + random.uniform(0, backoff)
            print(f"[RETRY] Rate‑Limit (429) – Versuch {attempt + 1}. Warte {wait:.2f}s …")
            time.sleep(wait)
            attempt += 1
        except Exception as e:
            raise e

RATING_RE  = re.compile(r"\[Rating]\s*([1-5])\s*\[/Rating]", re.I | re.S)
REASON_RE  = re.compile(r"\[Reason]\s*(.*?)\s*\[/Reason]", re.I | re.S)

def parse_response(text: str):
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
    if not os.getenv(ANTHROPIC_API_KEY_ENV_VAR):
        raise EnvironmentError(f"Fehlende Umgebungsvariable {ANTHROPIC_API_KEY_ENV_VAR}")
    print(f"[INFO] Verwende Anthropic-Modell: {LLM_MODEL}")
    with open(CSV_FILE_PATH, encoding="utf-8") as infile, open(OUT_PATH, "w", newline='', encoding="utf-8") as outfile:
        writer = csv.DictWriter(outfile, fieldnames=['submissionID', 'cluster_label', 'rating', 'reason', 'source_code', 'error_message', 'full_prompt'])
        writer.writeheader()
        reader = csv.DictReader(infile)
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
    print("─── STARTE CLUSTER-LABEL-EVALUATION (EDUCATIONAL) ───")
    try:
        main()
    finally:
        print("─── EVALUATION ABGESCHLOSSEN ───")
