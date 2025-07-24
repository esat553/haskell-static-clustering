| German Cluster Name                      | English Cluster Name                    | file_name                        | regex |
|------------------------------------------|-----------------------------------------|----------------------------------|-------|
| GHCi Kontext in Abgabe                   | GHCi Context in Submission              | ghci_context_in_submission       | (?m)(^|\n).*?(ghci>|Prelude>|parse error on input\s+‘(:\{|}:)’|:{|}:) |
| Ungültige Top-Level-Deklaration          | Invalid Top-Level Declaration           | invalid_top_level_declaration    | Parse error: module header, import declaration\s+or\s+top-level declaration expected\. |
| Parse-Fehler durch Import-Fehler         | Parse Error due to Import Error         | parse_error_import               | (could not find module|parse error on input)[\s\S]+?‘?import |
| Parse-Fehler in 'let'-Binding            | Parse Error in 'let' Binding            | parse_error_in_let_binding       | parse\s+error\s*\(possibly\s+incorrect\s+indentation[^\)]*\)[\s\S]*?\n\s*\d+\s*\|\s+.*\blet\b[^\n]*= |
| Parse-Fehler in Funktionsdeklaration     | Parse Error in Function Declaration     | parse_error_function_declaration | parse error.*?\n\s*\|\s*(\d+)\s\|\s([a-z]\w*)\s*:: |
| Parse-Fehler                            | Parse Error                            | parse_error                     | \bparse\s+error\b |
| Falsche Funktionsarität                  | Incorrect Function Arity                | incorrect_function_arity         | \bapplied to too (few|many) arguments\b|\bhas \w+ arguments, but its type .*? has only \w+ |
| Inkonsistenter Rückgabetyp               | Inconsistent Return Type                | inconsistent_return_type         | Couldn't match type '.*' with '.*'.*In an equation for|In a case alternative |
| Implementierung verletzt Typsignatur     | Implementation Violates Type Signature  | implementation_violates_signature| is a rigid type variable bound by |
| Numerischer Typenkonflikt                | Numeric Type Conflict                   | numeric_type_conflict            | No instance for \(Fractional\s+[A-Za-z0-9]+\)|No instance for \(Num\s+[A-Za-z0-9]+\)|Couldn't match expected type\s+‘?(Double|Float|Rational|Int|Integer|Num\s+[a-zA-Z0-9]*)’?\s+with actual type\s+‘?(Double|Float|Rational|Int|Integer|Num\s+[a-zA-Z0-9]*)’? |
| Doppelte Signatur                        | Duplicate Type Signature                | duplicate_type_signature         | duplicate type signatures? |
| Typenkonflikt                            | Type Mismatch                           | type_mismatch                    | couldn't? match (expected type|type) |
| Unendlicher Typ                          | Infinite Type                           | infinite_type                    | occurs check:.*infinite type |
| Mehrfache Deklarationen                  | Multiple Declarations                   | multiple_declarations            | multiple declarations |
| Mehrdeutiger Bezeichner                  | Ambiguous Identifier                    | ambiguous_identifier             | ambiguous occurrence |
| Ungültiger Typ-Operator                  | Illegal Type Operator                   | illegal_type_operator            | illegal operator .* in type .* |
| Typenkonstruktor oder Klasse nicht definiert | Type Constructor or Class Not Defined | type_constructor_not_defined     | Not in scope: type constructor or class ‘[A-Z][a-zA-Z0-9_']*’ |
| Nicht definierter Datenkonstruktor       | Data Constructor Not Defined             | data_constructor_not_defined     | Not in scope: data constructor ‘[A-Z][a-zA-Z0-9_']*’ |
| Funktion nicht definiert                 | Function Not Defined                     | function_not_defined             | Variable not in scope: ([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*[^:\n]+->.* |
| Variable nicht im Gültigkeitsbereich     | Variable Not in Scope                    | variable_not_in_scope            | not in scope |
| Pattern Binding in Instanz               | Pattern Binding in Instance Declaration  | pattern_binding_in_instance      | pattern bindings.*not allowed in instance declaration |
| Fehlendes Binding                        | Missing Binding                          | missing_binding                  | type signature.*lacks an accompanying binding |
| Falsche Arität für Konstruktor         F  | Wrong Constructor Arity                   | wrong_constructor_arity          | the constructor ‘.*’ should have \d+ argument[s]?, but has been given \d+ |
| Abweichende Arity                        | Inconsistent Arity                        | inconsistent_arity               | equations for .* have different numbers of arguments |
| Constraint erwartet, aber Typ erhalten   | Constraint Expected, Got Type              | constraint_expected_got_type     | expected a constraint, but .* has kind |
| Ungültige Typensignatur                  | Invalid Type Signature                     | invalid_type_signature           | (invalid|illegal) type signature |
| Überlappende Instanzen                   | Overlapping Instances                      | overlapping_instances            | overlapping instances for |
| Fehlende Constraint bei Funktionssignatur| Missing Constraint in Function Signature    | missing_constraint_signature     | No instance for \(\w+ [a-z]\) arising from a use of |
| Fehlende Superklassen-Instanz            | Missing Superclass Instance                 | missing_superclass_instance      | no\s+instance\s+for.*arising\s+from\s+the\s+superclasses |
| Fehlende Instanz                         | Missing Instance                            | missing_instance                 | no instance for |
| Fehlende Instanz bei 'deriving'          | Missing Instance in 'deriving'              | missing_instance_deriving        | When deriving the instance for |
| Konflikt in 'data'-Deklaration           | Conflict in 'data' Declaration              | data_declaration_conflict        | Conflicting definitions for\s+['‘`]?.+?['’`]?\s+.*?\n\s*\|\s*\n\s*\d+\s*\|\s*data |
| Mehrfachdefinition in Funktionsgleichung | Multiple Definitions in Function Equation   | multiple_function_definitions    | conflicting definitions for.*in an equation for |
| Konfliktierende Bindings                 | Conflicting Bindings                        | conflicting_bindings             | conflicting definitions for |
| Ungültige Instanz-Signatur               | Invalid Instance Signature                  | invalid_instance_signature       | illegal type signature in instance declaration |
| Modul nicht gefunden                     | Module Not Found                            | module_not_found                 | could not find module |
| Fehler mit Datenkonstruktoren            | Data Constructor Error                      | data_constructor_error           | (cannot parse data constructor in a data/newtype declaration|not a data constructor) |
| Mehrfache Instanzdeklaration             | Duplicate Instance Declaration              | duplicate_instance_declaration   | duplicate instance declarations |
| Methode nicht in Klasse                  | Method Not in Class                         | method_not_in_class              | is not a \(visible\) method of class |
| Ungültige Instanz-Form                   | Invalid Instance Form                       | invalid_instance_form            | illegal instance declaration|Use FlexibleInstances |
| Falsche Anzahl von Typ-Argumenten        | Wrong Number of Type Arguments              | wrong_number_of_type_arguments   | expecting one more argument to .*has kind |
| Kind-Konflikt                            | Kind Mismatch                               | kind_mismatch                    | expected kind .* but .* has kind |
| Kind-Konflikt (Constraint vs. Typ)       | Kind Mismatch (Constraint vs. Type)         | kind_mismatch_constraint_type    | expected (a constraint|a type), but .* has kind |
| Mehrdeutiger Typ                         | Ambiguous Type                              | ambiguous_type                   | ambiguous type variable |
| Constraint nicht erfüllbar               | Constraint Not Satisfiable                  | unsatisfiable_constraint         | could not deduce.*\( |
| Flexible Kontexte benötigt               | Flexible Contexts Required                  | flexible_contexts_required       | non type-variable argument in the constraint |
| Fehlende GADTs-Erweiterung               | Missing GADTs Extension                     | missing_gadts_extension          | enable the GADTs extension |
| Ungültiges Zeichen                       | Invalid Character                           | invalid_character                | syntax error |
| Lexikalischer Fehler                     | Lexical Error                               | lexical_error                    | lexical error at character |
| Fehlerhafter Typ-Header                  | Malformed Type Header                       | malformed_type_header            | malformed head of type or class declaration |
| Leerer do-Block                          | Empty 'do' Block                            | empty_do_block                   | empty\s+'do'\s+block |
| Letzte Anweisung im 'do'-Block           | Last Statement in 'do' Block                 | last_statement_in_do_block       | the last statement in a 'do' block must be an expression |
| Typed Hole                               | Typed Hole                                  | typed_hole                       | found hole: _ :: |
| Ungültige Binding-Syntax                 | Invalid Binding Syntax                      | invalid_binding_syntax           | illegal binding of built-in syntax |
| Ungültiges Enum-Deriving                 | Invalid Enum Deriving                       | invalid_enum_deriving            | can't make a derived instance of ['‘`]Enum |
| Ungültiges Deriving                      | Invalid Deriving                            | invalid_deriving                 | illegal deriving item |
| Warnung                                  | Warning                                     | warning                          | warning |
| Sonstiger Fehler                         | Other Error                                 | other_error                      | .* |
