"""
In the original Version this file was interacting with a database but for simplification the results are saved in a csv
"""
import os
import csv
import subprocess
import re
import concurrent.futures
from datetime import datetime
from pathlib import Path
from typing import TypedDict, Tuple, List

BASE_DIR = Path(__file__).resolve().parent
COMPILATION_RESULTS_DIR = BASE_DIR / 'compilation_results'
ARTIFICIAL_SUBMISSIONS_DIR = BASE_DIR / '..' / 'artificial_submissions'
DOCKER_IMAGE = 'safe-docker-ghc9.4.8'
CSV_FILE_PATH = COMPILATION_RESULTS_DIR / 'ghc9_4_8_results.csv'

PATH_METADATA_REGEX = re.compile(
    r'(human_generated|llm_generated)'
    r'[/]'
    r'(?:exercisesheet|lecture)_(\d+)'
)
FILENAME_METADATA_REGEX = re.compile(r'(?:human_error|llm_error)_(\d+)\.hs')

class CompilationResult(TypedDict):
    id: int
    vl_nr: int
    example_number: int
    error_source: str
    source_code: str
    compiler_output: str
    file_path: str
    created_at: str
    compiles: int

def process_file(args: Tuple[Path, str, int]) -> CompilationResult:
    """Compiles a single Haskell file inside a Docker container and captures the result."""
    file_path, image, entry_id = args

    match = PATH_METADATA_REGEX.search(str(file_path))
    if not match:
        raise ValueError(f"Could not extract metadata from file path: {file_path}")

    error_source = match.group(1)
    vl_nr = int(match.group(2))
    filename_match = FILENAME_METADATA_REGEX.match(file_path.name)
    if not filename_match:
        raise ValueError(f"Could not extract example number from filename: {file_path.name}")
    example_number = int(filename_match.group(1))

    output: str
    source_code = ""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            source_code = f.read()

        ghci_cmd = (
            "set -o pipefail && "
            "ghci -ignore-dot-ghci -v0 -ferror-spans -fdiagnostics-color=never -Wall "
            f"-e ':load /tmp/{file_path.name}' -e ':quit' 2>&1"
        )
        cmd = [
            "docker", "run", "--rm",
            "-v", f"{str(file_path.resolve())}:/tmp/{file_path.name}:ro",
            image,
            "bash", "-c", ghci_cmd
        ]
        
        proc = subprocess.run(cmd, text=True, capture_output=True, timeout=60, encoding="utf-8", errors="replace")
        
        raw_output = (proc.stdout or "") + (proc.stderr or "")
        output = re.sub(r"/tmp/.*?\.hs", "submission.hs", raw_output)
        
    except subprocess.TimeoutExpired:
        output = "Timeout: Compilation exceeded 60 seconds."
    except FileNotFoundError:
        output = f"Error: Source file not found at {file_path}"
    except Exception as e:
        output = f"Unexpected error during compilation: {str(e)}"

    return CompilationResult(
        id=entry_id,
        vl_nr=vl_nr,
        example_number=example_number,
        error_source=error_source,
        source_code=source_code,
        compiler_output=output,
        file_path=str(file_path),
        created_at=datetime.now().isoformat(),
        compiles=0 if "error:" in output else 1,
    )

def main():
    """Finds all Haskell files, processes them in parallel, and saves the results to a CSV file."""
    os.makedirs(COMPILATION_RESULTS_DIR, exist_ok=True)

    all_hs_files = ARTIFICIAL_SUBMISSIONS_DIR.rglob("*.hs")
    
    haskell_files: List[Path] = [
        f for f in all_hs_files
        if re.match(r"human_error_\d+\.hs", f.name) or re.match(r"llm_error_\d+\.hs", f.name)
    ]

    if not haskell_files:
        print("No Haskell files matching the required pattern found in the specified directory.")
        return

    file_count = len(haskell_files)
    print(f"Found {file_count} Haskell files to process.")

    tasks = [(path, DOCKER_IMAGE, i + 1) for i, path in enumerate(haskell_files)]
    
    csv_headers = list(CompilationResult.__annotations__.keys())

    with open(CSV_FILE_PATH, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=csv_headers)
        writer.writeheader()

        max_workers = os.cpu_count() or 1
        with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
            for i, result in enumerate(executor.map(process_file, tasks), 1):
                writer.writerow(result)
                print(f"[{i}/{file_count}] Processed: {result['file_path']}")

    print(f"\nProcessing complete. Results saved to '{CSV_FILE_PATH}'.")

if __name__ == "__main__":
    main()