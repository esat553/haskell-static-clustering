# GHCi Kontext in Abgabe (GHCi Context in Submission)

## Deutsche Beschreibung
Dieses Cluster fasst Parse-Fehler zusammen, die entstehen, wenn SchülerInnen ghci-spezifische Syntax wie Prompts (`ghci>`, `Prelude>`) oder REPL-Ausgaben versehentlich in die Haskell-Abgabe kopieren. Das ist ein typischer Anfängerfehler und verhindert, dass der Code korrekt kompiliert oder getestet werden kann.

## Mögliche Behebung (Fix)
Alle ghci-spezifischen Elemente (z. B. Prompts, REPL-Ausgaben, direkte Funktionsaufrufe) aus dem Code entfernen. Nur gültige Haskell-Deklarationen (z. B. `main`, `let`, `where`) im Top-Level-Bereich belassen.

## English Description
This cluster covers parse errors caused by students accidentally including ghci-specific syntax such as prompts (`ghci>`, `Prelude>`) or REPL outputs in their Haskell submissions. This is a common beginner’s mistake and prevents the code from compiling or being tested properly.

## Suggested Fix
Remove all ghci-specific elements (e.g., prompts, REPL output, direct function calls). Ensure that only valid Haskell declarations (e.g., `main`, `let`, `where`) remain in the top-level section of the code.

## Regex
```python
(?m)(^|\n).*?(ghci>|Prelude>|parse error on input\s+‘(:\{|}:)’|:{|}:)

```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
