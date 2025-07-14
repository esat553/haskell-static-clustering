# Unendlicher Typ (Infinite Type)

## Deutsche Beschreibung
Dieser Cluster beschreibt den logischen Fehler, bei dem der Compiler versucht, einen unendlichen Typen zu konstruieren, z.B. `a ~ [a]` (ein Typ `a` soll gleichzeitig eine Liste von sich selbst sein). Dies wird durch die Meldung `Occurs check: cannot construct the infinite type` signalisiert. Der Fehler entsteht, wenn Code eine Variable wie eine Funktion behandelt (z.B. `x y`), eine Funktion wie einen Wert (z.B. `f * 2`), oder eine Funktion auf einen "Container" (wie eine Liste oder `Maybe`) anwendet, anstatt auf den Wert *darin*.

## Mögliche Behebung (Fix)
Überprüfe die Anwendung von Funktionen und Operatoren. Stelle sicher, dass Variablen nicht fälschlicherweise als Funktionen aufgerufen werden. Wenn eine Funktion auf den Inhalt eines Containers wirken soll, muss der Wert zuerst "entpackt" werden (z.B. durch Pattern Matching), anstatt den gesamten Container an die Funktion zu übergeben.

## English Description
This cluster describes the logical error where the compiler tries to construct an infinite type, e.g., `a ~ [a]` (a type `a` would have to be a list of itself). This is indicated by the message `Occurs check: cannot construct the infinite type`. The error arises when code treats a variable like a function (e.g., `x y`), a function like a value (e.g., `f * 2`), or applies a function to a "container" (like a list or `Maybe`) instead of the value *inside* it.

## Suggested Fix
Check the application of functions and operators. Ensure that variables are not mistakenly called as functions. If a function is meant to operate on the contents of a container, the value must be "unpacked" first (e.g., through pattern matching), rather than passing the entire container to the function.
## Regex Pattern
```python
occurs check:.*infinite type
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
