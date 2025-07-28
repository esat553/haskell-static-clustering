#!/usr/bin/env python3
"""
Setup script for tree-sitter Haskell parser.
Run this script to build the tree-sitter library if you're having issues.
"""

import subprocess
import sys
from pathlib import Path
from tree_sitter import Language

def main():
    print("Setting up tree-sitter Haskell parser...")
    
    # Find the tree-sitter-haskell directory
    current_dir = Path.cwd()
    haskell_grammar_path = None
    
    for potential_path in [
        current_dir / "tree-sitter-haskell",
        current_dir.parent / "tree-sitter-haskell",
        current_dir / "../tree-sitter-haskell"
    ]:
        if potential_path.exists():
            haskell_grammar_path = potential_path.resolve()
            break
    
    if haskell_grammar_path is None:
        print("❌ tree-sitter-haskell not found!")
        print("Please run: git submodule update --init --recursive")
        sys.exit(1)
    
    print(f"✅ Found tree-sitter-haskell at: {haskell_grammar_path}")
    
    # Setup build directory
    project_root = haskell_grammar_path.parent
    build_dir = project_root / "build"
    build_dir.mkdir(exist_ok=True)
    lang_so = build_dir / "my-languages.so"
    
    # Build the language library
    try:
        print("🔨 Building tree-sitter library...")
        Language.build_library(str(lang_so), [str(haskell_grammar_path)])
        print(f"✅ Successfully built: {lang_so}")
        
        # Test loading
        print("🧪 Testing library loading...")
        language = Language(str(lang_so), "haskell")
        print("✅ Library loads successfully!")
        
    except Exception as e:
        print(f"❌ Failed to build/test library: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
