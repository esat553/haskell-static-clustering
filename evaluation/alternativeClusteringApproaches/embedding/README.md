### How to run the script and see the results

After installing the required packages from the requirements.txt you can run the following command:

Run the Skript with:
```bash
streamlit run analysis.py 
```

### Configurations

You can change the configurations for you own needs but delete the embadding_results file before running the script angain.

### tree-sitter

To run the AST-Based clustering you need to install the tree-sitter-haskell module. Execute the following commands:

1. First, initialize the submodule:
```bash
git submodule update --init --recursive
```

2. **IMPORTANT**: Install compatible py-tree-sitter version:
```bash
pip install tree-sitter==0.20.4
```

3. For AST-based clustering, run with Streamlit:
```bash
cd ../ast-based
streamlit run ast_experiment.py
```

4. To test AST parsing without Streamlit:
```bash
cd ../ast-based
python ast_experiment.py
```

5. If you're having issues with tree-sitter, try running the setup script:
```bash
cd ../ast-based
python setup_tree_sitter.py
```

6. Alternative manual build (only if step 2 didn't work):
```bash
cd tree-sitter-haskell
tree-sitter generate
cd ..
mkdir -p build
tree-sitter build --output build/haskell.so tree-sitter-haskell
```

### Troubleshooting tree-sitter

If you encounter issues with tree-sitter:

1. **First try the compatible version**:
```bash
pip uninstall tree-sitter
pip install tree-sitter==0.20.4
```

2. Check if tree-sitter CLI is installed (only needed for manual builds):
```bash
npm install -g tree-sitter-cli
```

3. Verify the submodule is properly initialized:
```bash
ls tree-sitter-haskell/
# Should show grammar files
```

### Important Notes

- **Always use `streamlit run ast_experiment.py`** for the web interface
- The script can also run in CLI mode for testing: `python ast_experiment.py`
- Use py-tree-sitter version 0.20.4 for compatibility with existing tree-sitter-haskell
- The setup_tree_sitter.py script is optional and only needed for troubleshooting