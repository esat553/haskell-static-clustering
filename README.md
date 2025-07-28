## Repository for Bachelor Thesis "Static Analysis for Automated Error Clustering of Haskell Programming Exercises"

> The code in this repository works on Linux where Docker and Python are installed. For running it on other operating systems, adjustments will have to be made.

This repository includes the codebase of the prototype, sample data, and documentation for the regular expression-based clustering presented in the Bachelor Thesis of Esat Akif Avci: "Static Analysis for Automated Error Clustering of Haskell Programming Exercises".

### Repository Overview

* **artificial_submissions**: An artificial dataset of submissions to evaluate the clustering
* **compilation**: The Python script to compile the artificial submissions and save the results as a CSV
* **evaluation**: The experiments to evaluate the clustering with the artificial submissions set
* **ghc_docker**: A Docker image with GHC to compile the code
* **implementation**: The code as implemented in GATE
* **list_of_clusters**: A list of all clusters with German and English explanations and an example for each cluster
* **prototype**: The prototype of the clustering system with a test script to test every cluster

### Setting up the Repository

This repository uses [git submodules](https://github.com/tree-sitter/tree-sitter-haskell.git). You can clone the repo with the standard cloning command, but if you want to include the submodule used for the AST-clustering, you need to recursively clone with the following command:

```bash
git clone --recursive [cloning link from above]
```

Or you can initialize and update later with:

```bash
git submodule update --init --recursive
```

For the repository, you must create a `.env` file in the root with credentials for the APIs from each LLM like the following:

```
GOOGLE_API_KEY=[API-Key]
CLAUDE_API_KEY=[API-Key]
OPENAI_API_KEY=[API-Key]
```

### Requirements

I used the current stable version for Python and LTS release for Java. The code will probably work for other versions as well.

* Python Version: 3.13.5
* Java Version: OpenJDK 21

### Python Dependencies

For every script requiring Python packages, a `requirements.txt` file is provided.

### Recommended Workflow

1. First, a compilation of the artificial submissions is recommended. You can do this by running the `compilation/compile.py` script.

2. Secondly, classifying the submissions with the `prototype/classify_artificial_submissions.py` file is recommended.

3. After this, every script should be runnable independently