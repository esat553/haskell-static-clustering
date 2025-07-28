#!/usr/bin/env python3
import csv
import os
from pathlib import Path

from de_regex_classification import classify

BASE_DIR = Path(__file__).resolve().parent
INPUT_CSV_PATH = BASE_DIR / '..' / 'compilation' / 'compilation_results' / 'ghc9_4_8_results.csv'
OUTPUT_DIR = BASE_DIR / 'clustering_results'
OUTPUT_CSV_PATH = OUTPUT_DIR / 'ghc9_4_8_clustering_results.csv'


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    print(f"Reading compilation data from: {INPUT_CSV_PATH}")
    
    output_headers = [
        'submissionID',
        'vl_nr',
        'example_number',
        'source_type',
        'detected_cluster',
        'source_code',
        'compiler_output',
        'created_at'
    ]
    
    with open(INPUT_CSV_PATH, mode='r', encoding='utf-8') as infile, \
         open(OUTPUT_CSV_PATH, mode='w', newline='', encoding='utf-8') as outfile:
        
        reader = csv.DictReader(infile)
        writer = csv.DictWriter(outfile, fieldnames=output_headers)
        
        writer.writeheader()
        
        for row in reader:
            if row.get('compiles', '1') != '0':
                continue
            compiler_output = row.get('compiler_output', '')
            detected_cluster = classify(compiler_output)
            output_row = {
                'submissionID': row['id'],
                'vl_nr': row['vl_nr'],
                'example_number': row['example_number'],
                'source_type': row['error_source'],
                'detected_cluster': detected_cluster,
                'source_code': row['source_code'],
                'compiler_output': compiler_output,
                'created_at': row['created_at']
            }
            writer.writerow(output_row)

    print("\nProcessing completed.")
    print(f"The results have been successfully saved to the following file:")
    print(OUTPUT_CSV_PATH)


if __name__ == "__main__":
    if not INPUT_CSV_PATH.is_file():
        print(f"Error: Input file not found: {INPUT_CSV_PATH}")
        print("Please make sure that the script 'compile.py' was executed first.")
    else:
        main()