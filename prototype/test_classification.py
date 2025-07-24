import pytest
import os
import subprocess
from collections import Counter
from de_regex_classification import classify, CLUSTERS

GHC_VERSION = "9.4.8"
IMAGE_NAME = f"safe-docker-ghc{GHC_VERSION}"
CLUSTER_BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'list_of_clusters'))
GHC_DOCKER_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'ghc_docker'))


def get_test_files():
    """Sammelt alle Testdateien aus dem `list_of_clusters`-Verzeichnis."""
    test_files = []
    if not os.path.isdir(CLUSTER_BASE_DIR):
        pytest.fail(f"Cluster-Verzeichnis nicht gefunden: {CLUSTER_BASE_DIR}")

    for cluster_name in os.listdir(CLUSTER_BASE_DIR):
        cluster_dir = os.path.join(CLUSTER_BASE_DIR, cluster_name)
        if os.path.isdir(cluster_dir):
            for filename in os.listdir(cluster_dir):
                if filename.endswith(".hs"):
                    filepath = os.path.join(cluster_dir, filename)
                    # Konvertiere den Verzeichnisnamen in den erwarteten Label-Namen
                    # z.B. 'ambiguous_identifier' -> 'Mehrdeutiger Bezeichner'
                    expected_label = cluster_name.replace('_', ' ').title()
                    test_files.append(pytest.param(filepath, expected_label, id=f"{cluster_name}-{filename}"))
    return test_files

def compile_with_ghc(filepath: str) -> str:
    """Kompiliert eine Haskell-Datei mit dem GHC-Docker-Container und gibt stderr zurück."""
    command = [
        "docker", "run", "--rm",
        "-v", f"{os.path.abspath(filepath)}:/tmp/src.hs:ro",
        IMAGE_NAME,
        "bash", "-c",
        (
            "ghci -ignore-dot-ghci -v0 -ferror-spans -fdiagnostics-color=never -fwarn-incomplete-patterns "
            "-e ':load /tmp/src.hs' -e ':quit' 2>&1 | sed 's#/tmp/src.hs#Studentenlösung#g'"
        )
    ]
    result = subprocess.run(command, capture_output=True, text=True, encoding='utf-8')
    return result.stdout


@pytest.fixture(scope="session", autouse=True)
def build_docker_image():
    """Stellt sicher, dass das Docker-Image vor den Tests gebaut wird."""
    # Überprüfen, ob das Image bereits existiert
    check_command = ["docker", "image", "inspect", IMAGE_NAME]
    if subprocess.run(check_command, capture_output=True).returncode == 0:
        print(f"Docker-Image '{IMAGE_NAME}' existiert bereits. Überspringe Build.")
        return

    print(f"Baue Docker-Image '{IMAGE_NAME}'...")
    build_script = os.path.join(GHC_DOCKER_DIR, 'build.sh')
    if not os.path.exists(build_script):
        pytest.fail(f"build.sh Skript nicht gefunden unter: {build_script}")
        
    result = subprocess.run([build_script, GHC_VERSION], capture_output=True, text=True, cwd=GHC_DOCKER_DIR)
    
    if result.returncode != 0:
        pytest.fail(f"Docker-Image-Build fehlgeschlagen:\n{result.stdout}\n{result.stderr}")
    
    print("Docker-Image erfolgreich gebaut.")


MANUAL_MAPPING = {
    "ambiguous_identifier": {"label": "Mehrdeutiger Bezeichner"},
    "ambiguous_type": {"label": "Mehrdeutiger Typ"},
    "conflicting_bindings": {"label": "Konfliktierende Bindings"},
    "constraint_expected_got_type": {"label": "Constraint erwartet, aber Typ erhalten"},
    "data_constructor_error": {"label": "Fehler mit Datenkonstruktoren"},
    "data_constructor_not_defined": {"label": "Nicht definierter Datenkonstruktor"},
    "data_declaration_conflict": {"label": "Konflikt in 'data'-Deklaration"},
    "duplicate_instance_declaration": {"label": "Mehrfache Instanzdeklaration"},
    "duplicate_type_signature": {"label": "Doppelte Signatur"},
    "empty_do_block": {"label": "Leerer do-Block"},
    "flexible_contexts_required": {"label": "Flexible Kontexte benötigt", "versions": ["8.*"]},
    "function_not_defined": {"label": "Funktion nicht definiert"},
    "ghci_context_in_submission": {"label": "GHCi Kontext in Abgabe"},
    "illegal_type_operator": {"label": "Ungültiger Typ-Operator", "versions": ["8.*"]},
    "implementation_violates_signature": {"label": "Implementierung verletzt Typsignatur"},
    "inconsistent_arity": {"label": "Abweichende Arity"},
    "inconsistent_return_type": {"label": "Inkonsistenter Rückgabetyp"},
    "incorrect_function_arity": {"label": "Falsche Funktionsarität"},
    "infinite_type": {"label": "Unendlicher Typ", "versions": ["8.*"]},
    "invalid_binding_syntax": {"label": "Ungültige Binding-Syntax",},
    "invalid_character": {"label": "Ungültiges Zeichen"},
    "invalid_deriving": {"label": "Ungültiges Deriving"},
    "invalid_enum_deriving": {"label": "Ungültiges Enum-Deriving"},
    "invalid_instance_form": {"label": "Ungültige Instanz-Form", "versions": ["8.*"]},
    "invalid_instance_signature": {"label": "Ungültige Instanz-Signatur", "versions":["8.*"]},
    "invalid_top_level_declaration": {"label": "Ungültige Top-Level-Deklaration"},
    "invalid_type_signature": {"label": "Ungültige Typensignatur"},
    "kind_mismatch": {"label": "Kind-Konflikt"},
    "kind_mismatch_constraint_type": {"label": "Kind-Konflikt (Constraint vs. Typ)"},
    "last_statement_in_do_block": {"label": "Letzte Anweisung im 'do'-Block"},
    "lexical_error": {"label": "Lexikalischer Fehler"},
    "malformed_type_header": {"label": "Fehlerhafter Typ-Header"},
    "method_not_in_class": {"label": "Methode nicht in Klasse"},
    "missing_binding": {"label": "Fehlendes Binding"},
    "missing_constraint_signature": {"label": "Fehlende Constraint bei Funktionssignatur"},
    "missing_gadts_extension": {"label": "Fehlende GADTs-Erweiterung", "versions":["8.*"]},
    "missing_instance": {"label": "Fehlende Instanz"},
    "missing_instance_deriving": {"label": "Fehlende Instanz bei 'deriving'"},
    "missing_superclass_instance": {"label": "Fehlende Superklassen-Instanz"},
    "module_not_found": {"label": "Modul nicht gefunden"},
    "multiple_declarations": {"label": "Mehrfache Deklarationen"},
    "multiple_function_definitions": {"label": "Mehrfachdefinition in Funktionsgleichung"},
    "numeric_type_conflict": {"label": "Numerischer Typenkonflikt"},
    "other_error": {"label": "Sonstiger Fehler"},
    "overlapping_instances": {"label": "Überlappende Instanzen"},
    "parse_error": {"label": "Parse-Fehler"},
    "parse_error_function_declaration": {"label": "Parse-Fehler in Funktionsdeklaration"},
    "parse_error_import": {"label": "Parse-Fehler durch Import-Fehler"},
    "parse_error_in_let_binding": {"label": "Parse-Fehler in 'let'-Binding"},
    "pattern_binding_in_instance": {"label": "Pattern Binding in Instanz"},
    "type_constructor_not_defined": {"label": "Typenkonstruktor oder Klasse nicht definiert"},
    "type_mismatch": {"label": "Typenkonflikt"},
    "typed_hole": {"label": "Typed Hole"},
    "unsatisfiable_constraint": {"label": "Constraint nicht erfüllbar"},
    "variable_not_in_scope": {"label": "Variable nicht im Gültigkeitsbereich"},
    "warning": {"label": "Warnung"},
    "wrong_constructor_arity": {"label": "Falsche Arität für Konstruktor"},
    "wrong_number_of_type_arguments": {"label": "Falsche Anzahl von Typ-Argumenten"},
}


def get_test_files_with_mapping():
    """Sammelt Testdateien und mappt Verzeichnisnamen zu deutschen Labels."""
    test_files = []
    if not os.path.isdir(CLUSTER_BASE_DIR):
        pytest.fail(f"Cluster-Verzeichnis nicht gefunden: {CLUSTER_BASE_DIR}")

    for cluster_name in os.listdir(CLUSTER_BASE_DIR):
        cluster_dir = os.path.join(CLUSTER_BASE_DIR, cluster_name)
        if os.path.isdir(cluster_dir) and cluster_name in MANUAL_MAPPING:
            cluster_info = MANUAL_MAPPING[cluster_name]
            expected_label = cluster_info["label"]
            
            for filename in os.listdir(cluster_dir):
                if filename.endswith(".hs"):
                    filepath = os.path.join(cluster_dir, filename)
                    
                    marks = []
                    xfail_versions = cluster_info.get("xfail_versions", {})
                    if GHC_VERSION in xfail_versions:
                        reason = xfail_versions[GHC_VERSION]
                        marks.append(pytest.mark.xfail(reason=reason, strict=True))

                    param = pytest.param(filepath, expected_label, cluster_info, id=f"{cluster_name}-{filename}", marks=marks)
                    test_files.append(param)
    return test_files


test_data = get_test_files_with_mapping()
classification_results = []
compatibility_warnings = []

@pytest.mark.parametrize("filepath, expected_label, cluster_info", test_data)
def test_single_file_classification(filepath, expected_label, cluster_info):
    """Testet die Klassifikation für eine einzelne Haskell-Datei."""
    required_versions = cluster_info.get("versions")
    if required_versions and GHC_VERSION not in required_versions:
        warning_message = (
            f"SKIPPED: Cluster '{expected_label}' ist nur für GHC-Version(en) "
            f"{required_versions} relevant (aktuell: {GHC_VERSION})."
        )
        compatibility_warnings.append(warning_message)
        pytest.skip(warning_message)

    stderr = compile_with_ghc(filepath)
    assert stderr, f"Kompilierung von {filepath} erzeugte keine Fehlermeldung."
    
    classified_label = classify(stderr)
    classification_results.append(classified_label)
    
    assert classified_label == expected_label, \
        f"Fehler bei Datei: {os.path.basename(filepath)}\n" \
        f"Erwartet: '{expected_label}', Erhalten: '{classified_label}'\n" \
        f"--- GHC-Fehlermeldung ---\n{stderr}"

def test_cluster_coverage():
    """
    Überprüft, ob alle definierten Cluster (außer den letzten beiden)
    mindestens einmal von den Tests getroffen wurden.
    """
    pytest.main(args=['-q', '--collect-only']) 
    
    all_defined_clusters = list(CLUSTERS.keys())
    clusters_to_check = set(all_defined_clusters[:-2])
    
    used_clusters = set(classification_results)
    
    missing_clusters = clusters_to_check - used_clusters
    
    assert not missing_clusters, \
        f"Die folgenden Cluster wurden in den Tests nicht abgedeckt: {sorted(list(missing_clusters))}"

@pytest.fixture(scope="session", autouse=True)
def print_compatibility_warnings(request):
    """Gibt am Ende der Tests eine Zusammenfassung der Kompatibilitäts-Warnungen aus."""
    yield
    if compatibility_warnings:
        print("\n\n--- Zusammenfassung der Kompatibilitäts-Warnungen ---")
        for warning in compatibility_warnings:
            print(warning)
        print("----------------------------------------------------")
