import re
from collections import OrderedDict

CLUSTERS = OrderedDict([
    # Parse-Fehler
    ("GHCi Kontext in Abgabe", re.compile(r"(?m)(^|\n).*?(ghci>|Prelude>|parse error on input\s+‘(:\{|}:)’|:{|}:)", re.MULTILINE)),
    ("Ungültige Top-Level-Deklaration", re.compile(r"Parse error: module header, import declaration\s+or\s+top-level declaration expected\.", re.IGNORECASE)),
    ("Parse-Fehler durch Import-Fehler", re.compile(r"(parse error on input)[\s\S]+?‘?import",re.IGNORECASE)),
    ("Parse-Fehler in 'let'-Binding", re.compile(
        r"(?:\(let.*in.*\)-syntax\s+in\s+pattern"
        r"|parse\s+error\s*\(possibly\s+incorrect\s+indentation[^\)]*\)"
        r"[\s\S]*?\n\s*\d+\s*\|\s+.*\blet\b[^\n]*=)",
        re.IGNORECASE | re.DOTALL
    )),
    ("Parse-Fehler in Funktionsdeklaration", re.compile(r"parse error.*?\n\s*\|\s*(\d+)\s\|\s([a-z]\w*)\s*::", re.DOTALL | re.IGNORECASE)),
    ("Parse-Fehler", re.compile(r"\bparse\s+error\b", re.IGNORECASE)),

    # Typed Hole
    ("Typed Hole", re.compile(r"found hole: _ ::", re.IGNORECASE)),

    # Typ-Fehler
    ("Falsche Funktionsarität", re.compile(
        r"applied to too (?:few|many) value arguments"
        r"|applied to \w+ value arguments,.*?\bbut its type.*?has only \w+"
        r"|\bhas \w+ arguments, but its type .*? has only \w+"
        r"|is applied to .*? (?:visible )?arguments,.*?but its type .*? has only",
        re.IGNORECASE | re.DOTALL
    )),
    ("Inkonsistenter Rückgabetyp", re.compile(r"Couldn't match type[:\s]*.*with[:\s]*.*In a case alternative", re.DOTALL | re.IGNORECASE)),
    ("Implementierung verletzt Typsignatur", re.compile(r"is a rigid type variable bound by", re.DOTALL | re.IGNORECASE)),
    ("Doppelte Signatur", re.compile(r"duplicate type signatures?", re.IGNORECASE)),
    ("Unendlicher Typ", re.compile(r"occurs check:.*infinite type", re.IGNORECASE | re.DOTALL)),
    ("Mehrfache Deklarationen", re.compile(r"multiple declarations", re.IGNORECASE)),
    ("Mehrdeutiger Bezeichner", re.compile(r"ambiguous occurrence", re.IGNORECASE)),
    ("Numerischer Typenkonflikt", re.compile(
        r"No instance for .*Num .*"
        r"|No instance for .*Fractional .*"
        r"|Couldn't match expected type\s+.(Double|Float|Rational|Int|Integer|Num\s+a\d*).\s+with actual type\s+.(Double|Float|Rational|Int|Integer|Num\s+a\d*).",
        re.IGNORECASE | re.DOTALL
    )),
    ("Typenkonflikt", re.compile(r"couldn'?t match (expected type|type)", re.IGNORECASE)),
    ("Ungültiger Typ-Operator", re.compile(r"illegal operator .* in type .*", re.IGNORECASE)),

    # Not in scope / Gültigkeitsbereich
    ("Typenkonstruktor oder Klasse nicht definiert", re.compile(r"Not in scope: type constructor or class ‘[A-Z][a-zA-Z0-9_']*’", re.DOTALL | re.IGNORECASE)),
    ("Nicht definierter Datenkonstruktor", re.compile(r"Not in scope: data constructor ‘[A-Z][a-zA-Z0-9_']*’", re.DOTALL | re.IGNORECASE)),
    ("Funktion nicht definiert", re.compile(r"Variable not in scope: ([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*[^:\n]+->.*", re.DOTALL | re.IGNORECASE)),
    ("Variable nicht im Gültigkeitsbereich", re.compile(r"not in scope", re.IGNORECASE)),

    # Binding-Fehler
    ("Pattern Binding in Instanz", re.compile(r"pattern bindings.*not allowed in instance declaration", re.IGNORECASE)),
    ("Fehlendes Binding", re.compile(r"type signature.*lacks an accompanying binding", re.IGNORECASE | re.DOTALL)),

    # Konstruktoren / Arity
    ("Falsche Arität für Konstruktor", re.compile(
        r"the (?:data )?constructor .* should have \d+ argument[s]?, but has been given \d+",
        re.IGNORECASE | re.DOTALL)),
    ("Abweichende Arity", re.compile(r"equations for .* have different numbers of arguments", re.IGNORECASE | re.DOTALL)),
    ("Constraint erwartet, aber Typ erhalten", re.compile(r"expected a constraint, but .*(has kind|is a type)",re.IGNORECASE | re.DOTALL)),
    ("Ungültige Instanz-Signatur", re.compile(r"illegal type signature in instance declaration", re.IGNORECASE)),
    ("Ungültige Typensignatur", re.compile(r"((invalid|illegal) type signature|Invalid data constructor .* in type signature)",re.IGNORECASE)),

    # Instanz-Fehler
    ("Überlappende Instanzen", re.compile(r"overlapping instances for", re.IGNORECASE)),
    ("Fehlende Constraint bei Funktionssignatur", re.compile(r"No instance for [\(‘]\w+ [a-z][\)’] arising from a use of", re.IGNORECASE | re.DOTALL)),
    ("Fehlende Superklassen-Instanz", re.compile(r"no\s+instance\s+for.*arising\s+from\s+the\s+superclasses", re.IGNORECASE | re.DOTALL)),
    ("Fehlende Instanz bei 'deriving'", re.compile(r"When deriving the instance for", re.IGNORECASE)),
    ("Fehlende Instanz", re.compile(r"no instance for", re.IGNORECASE)),

    # Conflicts
    ("Konflikt in 'data'-Deklaration", re.compile(r"Conflicting definitions for\s+['‘`]?.+?['’`]?\s+.*?\n\s*\|\s*\n\s*\d+\s*\|\s*data", re.IGNORECASE | re.DOTALL)),
    ("Mehrfachdefinition in Funktionsgleichung", re.compile(r"conflicting definitions for.*in an equation for",re.IGNORECASE | re.DOTALL)),
    ("Konfliktierende Bindings", re.compile(r"conflicting definitions for", re.IGNORECASE)),

    # Module & Sonstiges
    ("Modul nicht gefunden", re.compile(r"could not find module", re.IGNORECASE)),
    ("Fehler mit Datenkonstruktoren", re.compile(r"(cannot parse data constructor in a data/newtype declaration|not a data constructor)", re.IGNORECASE)),
    ("Mehrfache Instanzdeklaration", re.compile(r"duplicate instance declarations", re.IGNORECASE)),
    ("Methode nicht in Klasse", re.compile(r"is not a \(visible\) method of class", re.IGNORECASE)),
    ("Ungültige Instanz-Form", re.compile(r"illegal instance declaration|Use FlexibleInstances", re.IGNORECASE)),
    ("Falsche Anzahl von Typ-Argumenten", re.compile(r"expecting one more argument to .*has kind", re.IGNORECASE | re.DOTALL)),
    ("Kind-Konflikt", re.compile(r"expected kind .* but .* has kind", re.IGNORECASE | re.DOTALL)),
    ("Kind-Konflikt (Constraint vs. Typ)", re.compile(r"expected (a constraint|a type), but .* (?:has kind|is a (?:constraint|type))",re.IGNORECASE | re.DOTALL)),
    ("Mehrdeutiger Typ", re.compile(r"ambiguous type variable", re.IGNORECASE)),
    ("Constraint nicht erfüllbar", re.compile(r"could not deduce", re.IGNORECASE)),
    ("Flexible Kontexte benötigt", re.compile(r"non type-variable argument in the constraint", re.IGNORECASE)),
    ("Fehlende GADTs-Erweiterung", re.compile(r"enable the GADTs extension", re.IGNORECASE)),
    ("Ungültiges Zeichen", re.compile(r"syntax error", re.IGNORECASE)),
    ("Lexikalischer Fehler", re.compile(r"(lexical error at character|Unicode character .+ looks like .+ but it is not)",re.IGNORECASE)),
    ("Fehlerhafter Typ-Header", re.compile(r"malformed head of type or class declaration", re.IGNORECASE)),
    ("Leerer do-Block", re.compile(r"empty\s+'do'\s+block", re.IGNORECASE)),
    ("Letzte Anweisung im 'do'-Block", re.compile(r"the last statement in a 'do' block must be an expression", re.IGNORECASE)),
    ("Ungültige Binding-Syntax", re.compile(r"illegal binding of built-in syntax", re.IGNORECASE)),
    ("Ungültige Binding-Syntax", re.compile(r"illegal binding of (?:built-in syntax|an existing name)",
    re.IGNORECASE)),
    ("Ungültiges Enum-Deriving", re.compile(
        r"can't make a derived instance of [‘'`]?Enum\b"
        r"|Can't make a derived instance of [‘'`]?Enum\b",
        re.IGNORECASE
    )),
    ("Ungültiges Deriving", re.compile(r"illegal deriving item", re.IGNORECASE)),

    # Warnungen und Rest
    ("Warnung", re.compile(r"warning", re.IGNORECASE)),
    ("Sonstiger Fehler", re.compile(r".*", re.DOTALL))
])

def classify(stderr: str) -> str:
    for label, pattern in CLUSTERS.items():
        if pattern.search(stderr):
            return label
    return "Fehler"
