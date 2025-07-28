// NOT YET IMPLEMENTED!!!

package implementation;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.TimeUnit;

public class RegExTestRunner {
    private static final String GHC_VERSION = "9.4.8";
    private static final String IMAGE_NAME = "safe-docker-ghc" + GHC_VERSION;
    private static final Map<String, String> CLUSTER_MAPPING = new HashMap<>();
    
    static {
        // Mapping from directory names to German labels
        CLUSTER_MAPPING.put("ambiguous_identifier", "Mehrdeutiger Bezeichner");
        CLUSTER_MAPPING.put("ambiguous_type", "Mehrdeutiger Typ");
        CLUSTER_MAPPING.put("conflicting_bindings", "Konfliktierende Bindings");
        CLUSTER_MAPPING.put("constraint_expected_got_type", "Constraint erwartet, aber Typ erhalten");
        CLUSTER_MAPPING.put("data_constructor_error", "Fehler mit Datenkonstruktoren");
        CLUSTER_MAPPING.put("data_constructor_not_defined", "Nicht definierter Datenkonstruktor");
        CLUSTER_MAPPING.put("data_declaration_conflict", "Konflikt in 'data'-Deklaration");
        CLUSTER_MAPPING.put("duplicate_instance_declaration", "Mehrfache Instanzdeklaration");
        CLUSTER_MAPPING.put("duplicate_type_signature", "Doppelte Signatur");
        CLUSTER_MAPPING.put("empty_do_block", "Leerer do-Block");
        CLUSTER_MAPPING.put("function_not_defined", "Funktion nicht definiert");
        CLUSTER_MAPPING.put("ghci_context_in_submission", "GHCi Kontext in Abgabe");
        CLUSTER_MAPPING.put("implementation_violates_signature", "Implementierung verletzt Typsignatur");
        CLUSTER_MAPPING.put("inconsistent_arity", "Abweichende Arity");
        CLUSTER_MAPPING.put("inconsistent_return_type", "Inkonsistenter Rückgabetyp");
        CLUSTER_MAPPING.put("incorrect_function_arity", "Falsche Funktionsarität");
        CLUSTER_MAPPING.put("invalid_binding_syntax", "Ungültige Binding-Syntax");
        CLUSTER_MAPPING.put("invalid_character", "Ungültiges Zeichen");
        CLUSTER_MAPPING.put("invalid_deriving", "Ungültiges Deriving");
        CLUSTER_MAPPING.put("invalid_enum_deriving", "Ungültiges Enum-Deriving");
        CLUSTER_MAPPING.put("invalid_top_level_declaration", "Ungültige Top-Level-Deklaration");
        CLUSTER_MAPPING.put("invalid_type_signature", "Ungültige Typensignatur");
        CLUSTER_MAPPING.put("kind_mismatch", "Kind-Konflikt");
        CLUSTER_MAPPING.put("kind_mismatch_constraint_type", "Kind-Konflikt (Constraint vs. Typ)");
        CLUSTER_MAPPING.put("last_statement_in_do_block", "Letzte Anweisung im 'do'-Block");
        CLUSTER_MAPPING.put("lexical_error", "Lexikalischer Fehler");
        CLUSTER_MAPPING.put("malformed_type_header", "Fehlerhafter Typ-Header");
        CLUSTER_MAPPING.put("method_not_in_class", "Methode nicht in Klasse");
        CLUSTER_MAPPING.put("missing_binding", "Fehlendes Binding");
        CLUSTER_MAPPING.put("missing_constraint_signature", "Fehlende Constraint bei Funktionssignatur");
        CLUSTER_MAPPING.put("missing_instance", "Fehlende Instanz");
        CLUSTER_MAPPING.put("missing_instance_deriving", "Fehlende Instanz bei 'deriving'");
        CLUSTER_MAPPING.put("missing_superclass_instance", "Fehlende Superklassen-Instanz");
        CLUSTER_MAPPING.put("module_not_found", "Modul nicht gefunden");
        CLUSTER_MAPPING.put("multiple_declarations", "Mehrfache Deklarationen");
        CLUSTER_MAPPING.put("multiple_function_definitions", "Mehrfachdefinition in Funktionsgleichung");
        CLUSTER_MAPPING.put("numeric_type_conflict", "Numerischer Typenkonflikt");
        CLUSTER_MAPPING.put("other_error", "Sonstiger Fehler");
        CLUSTER_MAPPING.put("overlapping_instances", "Überlappende Instanzen");
        CLUSTER_MAPPING.put("parse_error", "Parse-Fehler");
        CLUSTER_MAPPING.put("parse_error_function_declaration", "Parse-Fehler in Funktionsdeklaration");
        CLUSTER_MAPPING.put("parse_error_import", "Parse-Fehler durch Import-Fehler");
        CLUSTER_MAPPING.put("parse_error_in_let_binding", "Parse-Fehler in 'let'-Binding");
        CLUSTER_MAPPING.put("pattern_binding_in_instance", "Pattern Binding in Instanz");
        CLUSTER_MAPPING.put("type_constructor_not_defined", "Typenkonstruktor oder Klasse nicht definiert");
        CLUSTER_MAPPING.put("type_mismatch", "Typenkonflikt");
        CLUSTER_MAPPING.put("typed_hole", "Typed Hole");
        CLUSTER_MAPPING.put("unsatisfiable_constraint", "Constraint nicht erfüllbar");
        CLUSTER_MAPPING.put("variable_not_in_scope", "Variable nicht im Gültigkeitsbereich");
        CLUSTER_MAPPING.put("warning", "Warnung");
        CLUSTER_MAPPING.put("wrong_constructor_arity", "Falsche Arität für Konstruktor");
        CLUSTER_MAPPING.put("wrong_number_of_type_arguments", "Falsche Anzahl von Typ-Argumenten");
    }
    
    public static void main(String[] args) {
        RegExTestRunner runner = new RegExTestRunner();
        try {
            runner.runTests();
        } catch (Exception e) {
            System.err.println("Error running tests: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    public void runTests() throws IOException {
        Path clusterBaseDir = Paths.get("list_of_clusters").toAbsolutePath();
        
        if (!Files.exists(clusterBaseDir)) {
            System.err.println("Cluster directory not found: " + clusterBaseDir);
            return;
        }
        
        int totalTests = 0;
        int passedTests = 0;
        int failedTests = 0;
        List<String> failures = new ArrayList<>();
        
        System.out.println("Running Java regex classification tests...");
        System.out.println("Cluster base directory: " + clusterBaseDir);
        
        for (Path clusterDir : Files.list(clusterBaseDir).filter(Files::isDirectory).toArray(Path[]::new)) {
            String clusterName = clusterDir.getFileName().toString();
            String expectedLabel = CLUSTER_MAPPING.get(clusterName);
            
            if (expectedLabel == null) {
                System.out.println("Skipping unmapped cluster: " + clusterName);
                continue;
            }
            
            for (Path haskellFile : Files.list(clusterDir).filter(p -> p.toString().endsWith(".hs")).toArray(Path[]::new)) {
                totalTests++;
                
                try {
                    String stderr = compileWithGhc(haskellFile);
                    if (stderr.trim().isEmpty()) {
                        System.out.println("WARNING: No error output for " + haskellFile.getFileName());
                        continue;
                    }
                    
                    String classifiedLabel = RegExClassification.classify(stderr);
                    
                    if (expectedLabel.equals(classifiedLabel)) {
                        passedTests++;
                        System.out.println("✓ PASS: " + clusterName + "/" + haskellFile.getFileName() + " -> " + classifiedLabel);
                    } else {
                        failedTests++;
                        String failure = String.format("✗ FAIL: %s/%s\n  Expected: %s\n  Got: %s\n  Error: %s\n", 
                            clusterName, haskellFile.getFileName(), expectedLabel, classifiedLabel, 
                            stderr.length() > 200 ? stderr.substring(0, 200) + "..." : stderr);
                        failures.add(failure);
                        System.out.println(failure);
                    }
                } catch (Exception e) {
                    failedTests++;
                    String failure = "✗ ERROR: " + clusterName + "/" + haskellFile.getFileName() + " - " + e.getMessage();
                    failures.add(failure);
                    System.out.println(failure);
                }
            }
        }
        
        System.out.println("\n" + "=".repeat(50));
        System.out.println("TEST SUMMARY");
        System.out.println("=".repeat(50));
        System.out.println("Total tests: " + totalTests);
        System.out.println("Passed: " + passedTests);
        System.out.println("Failed: " + failedTests);
        System.out.println("Success rate: " + String.format("%.1f%%", (double) passedTests / totalTests * 100));
        
        if (!failures.isEmpty()) {
            System.out.println("\nFAILED TESTS:");
            System.out.println("-".repeat(30));
            for (String failure : failures) {
                System.out.println(failure);
            }
        }
    }
    
    private String compileWithGhc(Path haskellFile) throws IOException, InterruptedException {
        String absolutePath = haskellFile.toAbsolutePath().toString();
        
        ProcessBuilder pb = new ProcessBuilder(
            "docker", "run", "--rm",
            "-v", absolutePath + ":/tmp/src.hs:ro",
            IMAGE_NAME,
            "bash", "-c",
            "ghci -ignore-dot-ghci -v0 -ferror-spans -fdiagnostics-color=never -fwarn-incomplete-patterns " +
            "-e ':load /tmp/src.hs' -e ':quit' 2>&1 | sed 's#/tmp/src.hs#Studentenlösung#g'"
        );
        
        Process process = pb.start();
        
        StringBuilder output = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                output.append(line).append("\n");
            }
        }
        
        boolean finished = process.waitFor(30, TimeUnit.SECONDS);
        if (!finished) {
            process.destroyForcibly();
            throw new RuntimeException("Docker process timed out");
        }
        
        return output.toString();
    }
}
