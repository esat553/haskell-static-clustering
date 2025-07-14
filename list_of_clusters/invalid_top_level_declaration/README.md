# Ungültige Top-Level-Deklaration (Invalid Top-Level Declaration)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im Code ein ungültiger Ausdruck steht, den der Compiler nicht als gültige Top-Level-Deklaration interpretieren kann. Häufig geschieht das, wenn StudentInnen natürliche Sprache unkommentiert in den Code schreiben oder versehentlich eine Funktion direkt im Top-Level-Kontext aufrufen. Der Compiler erwartet an dieser Stelle jedoch z. B. Moduldefinitionen, Import-Anweisungen oder Funktionsdeklarationen.

## Mögliche Behebung (Fix)
Überprüfe, ob der betroffene Ausdruck eine gültige Haskell-Deklaration ist. Falls es sich um eine Bemerkung handelt, kommentiere sie korrekt mit `--`. Funktionsaufrufe oder Ausdrücke sollten innerhalb von `main` oder einem anderen gültigen Block stehen, nicht direkt im Top-Level.

## English Description
This cluster covers parse errors caused by invalid expressions at the top level of a Haskell file. These errors often occur when students include natural language in the code without commenting it out, or when they accidentally call a function directly at the top level. In such cases, the compiler expects a module header, import declaration, or function definition instead.

## Suggested Fix
Check whether the expression is a valid Haskell declaration. If it's a comment, prefix it with `--`. Function calls or expressions should be placed inside `main` or another valid context, not directly at the top level.

## Regex
```python
Parse error: module header, import declaration\s+or\s+top-level declaration expected\.
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs) and [example_2.hs](./example_2.hs).
