import csv, json, os, re, time, random, anthropic
from dotenv import load_dotenv
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime

load_dotenv()
LLM_MODEL = "claude-sonnet-4-20250514"
API_KEY = os.getenv("CLAUDE_API_KEY")
PROMPT_FILE = os.path.join(os.path.dirname(__file__), "en_prompt.json")
MAX_WORKERS = 3
MAX_SUBMISSIONS = 250
RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"llm_as_judge_sonnet_4.csv")
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'prototype', 'clustering_results', 'ghc9_4_8_clustering_results.csv')

client = anthropic.Anthropic(api_key=API_KEY)

def generate_prompt(src, err, label):
    with open(PROMPT_FILE, "r", encoding="utf-8") as f:
        p = json.load(f)
    prompt = f"""
{p['role']}
{p['instruction']}

{p['source_code_label']}:
{src}

{p['compiler_output_label']}:
{err}

{p['cluster_label']}: {label}

{p['output_format']}
"""
    temperature = p.get("temperature", 0.0)
    return prompt.strip(), temperature

def ask_llm(model_name, temperature, prompt, backoff=2):
    attempt = 0
    while True:
        try:
            response = client.messages.create(
                model=model_name,
                max_tokens=1024,
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

def parse_response(text):
    rating_match = RATING_RE.search(text)
    reason_match = REASON_RE.search(text)
    if not rating_match or not reason_match:
        raise ValueError("Antwort konnte nicht geparst werden")
    return int(rating_match.group(1)), reason_match.group(1).strip()

def remove_ground_truth_metadata(src):
    # The comments resulting from the manual analysis must be removed because they majorly influence the outcomes of the experiment.
    src = re.sub(r'\{\-.*?\-\}', '', src, flags=re.DOTALL)
    src = re.sub(r'--.*', '', src) 
    return src

def main():
    print(f"[INFO] Verwende Anthropic-Modell: {LLM_MODEL}")
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
