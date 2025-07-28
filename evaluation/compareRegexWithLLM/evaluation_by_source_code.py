import csv, json, os, google.generativeai as genai
from dotenv import load_dotenv
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
import re

load_dotenv()
LLM_MODEL = "gemini-2.5-flash"
API_KEY = os.getenv("GOOGLE_API_KEY")
MAX_WORKERS = 10
CSV_FILE_PATH = os.path.join(os.path.dirname(__file__), '..', '..', 'compilation', 'compilation_results', 'ghc9_4_8_results.csv')
RESULTS_DIR = os.path.join(os.path.dirname(__file__), 'results')
os.makedirs(RESULTS_DIR, exist_ok=True)
OUT_PATH = os.path.join(RESULTS_DIR, f"llm_categorization_sc.csv")

def remove_ground_truth_metadata(src):
    # The comments resulting from the manual analysis must be removed because they majorly influence the outcomes of the experiment.
    src = re.sub(r'\{\-.*?\-\}', '', src, flags=re.DOTALL)
    src = re.sub(r'--.*', '', src) 
    return src

def get_prompt(source_code):
    with open("./de_prompt_source_code.json", encoding="utf-8") as f:
        p = json.load(f)
    cats = "\n".join(f"- {c}" for c in p['category_list'])
    src_clean = remove_ground_truth_metadata(source_code)
    return f"{p['system_prompt']}\n{p['instruction']}\n{p['headline_category_list']}\n{cats}\n{p['headline_analyzation_string']}\n---\n{src_clean}"

def analyze(model, prompt):
    try:
        r = model.generate_content(contents=prompt, generation_config=genai.types.GenerationConfig(temperature=0.0))
        if hasattr(r, 'prompt_feedback') and r.prompt_feedback and r.prompt_feedback.block_reason:
            return "LLM_PROMPT_BLOCKED"
        return getattr(r, 'text', '').strip() or "LLM_EMPTY_RESPONSE"
    except Exception:
        return "LLM_ANALYSIS_FAILED"

def main():
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel(LLM_MODEL)
    with open(CSV_FILE_PATH, encoding="utf-8") as infile, open(OUT_PATH, "w", newline='', encoding="utf-8") as outfile:
        reader = csv.DictReader(infile)
        writer = csv.DictWriter(outfile, fieldnames=['submissionID', 'error_message', 'source_code', 'detected_cluster', 'full_prompt'])
        writer.writeheader()
        rows = [(row['id'], row['compiler_output'], row['source_code']) for row in reader if row.get('compiles', '1') == '0']
        total = len(rows)
        def worker(row):
            sid, err, src = row
            prompt = get_prompt(src)
            cluster = analyze(model, prompt)
            return {'submissionID': sid, 'error_message': err, 'source_code': src, 'detected_cluster': cluster, 'full_prompt': prompt}
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            for idx, result in enumerate(executor.map(worker, rows), 1):
                writer.writerow(result)
                print(f"[{idx}/{total}] verarbeitet: submissionID {result['submissionID']}")

if __name__ == "__main__":
    main()