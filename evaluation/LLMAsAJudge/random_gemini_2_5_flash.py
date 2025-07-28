import csv, json, os, re, time, random
from dotenv import load_dotenv
from concurrent.futures import ThreadPoolExecutor, as_completed
import google.generativeai as genai

load_dotenv()
LLM_MODEL = "gemini-2.5-flash"
API_KEY = os.getenv("GOOGLE_API_KEY")
PROMPT_FILE = os.path.join(os.path.dirname(__file__), "en_prompt.json")
CATEGORIES_LIST_FILE = os.path.join(os.path.dirname(__file__), "de_category_list.json")
MAX_WORKERS = 10
MAX_SUBMISSIONS = 250
RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"llm_as_judge_random_gemini_2_5_flash.csv")
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'prototype', 'clustering_results', 'ghc9_4_8_clustering_results.csv')

def get_random_cluster_label():
    with open(CATEGORIES_LIST_FILE, "r", encoding="utf-8") as f:
        cats = json.load(f)
    cluster_labels = cats.get("category_list", [])
    return random.choice(cluster_labels)

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

def ask_llm(model, temperature, prompt, backoff=2):
    attempt = 0
    while True:
        try:
            r = model.generate_content(
                prompt,
                generation_config=genai.types.GenerationConfig(temperature=temperature)
            )
            if hasattr(r, 'prompt_feedback') and r.prompt_feedback and r.prompt_feedback.block_reason:
                raise RuntimeError(f"Prompt blocked: {r.prompt_feedback.block_reason}")
            return r.text.strip()
        except Exception as e:
            wait = backoff * (2 ** attempt)
            print(f"[RETRY] Fehler: {e} – Versuch {attempt + 1}. Warte {wait:.2f}s …")
            time.sleep(wait)
            attempt += 1

RATING_RE  = re.compile(r"\[Rating]\s*([1-5])\s*\[/Rating]", re.I | re.S)
REASON_RE  = re.compile(r"\[Reason]\s*(.*?)\s*\[/Reason]", re.I | re.S)

def parse_response(text):
    rating_match = RATING_RE.search(text)
    reason_match = REASON_RE.search(text)
    if not rating_match or not reason_match:
        return 0, text
    return int(rating_match.group(1)), reason_match.group(1).strip()

def remove_ground_truth_metadata(src):
    src = re.sub(r'\{\-.*?\-\}', '', src, flags=re.DOTALL)
    src = re.sub(r'--.*', '', src)
    return src

def main():
    print(f"[INFO] Verwende Gemini-Modell: {LLM_MODEL}")
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel(LLM_MODEL)
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
            sid, _, src, err = row
            random_label = get_random_cluster_label()
            src_clean = remove_ground_truth_metadata(src)
            prompt, temperature = generate_prompt(src_clean, err, random_label)
            raw_response = ask_llm(model, temperature, prompt)
            rating, reason = parse_response(raw_response)
            return {
                'submissionID': sid,
                'cluster_label': random_label,
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
    print("─── STARTE CLUSTER-LABEL-EVALUATION (RANDOM GEMINI) ───")
    try:
        main()
    finally:
        print("─── EVALUATION ABGESCHLOSSEN ───")
