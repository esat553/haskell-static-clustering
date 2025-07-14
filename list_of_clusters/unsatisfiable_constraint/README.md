# Constraint nicht erfüllbar (Constraint Not Satisfiable)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn für eine Funktion oder Instanz eine notwendige Typklasse nicht aus dem Kontext hergeleitet werden kann. Häufig fehlt eine Constraint wie `Ord a` oder `Integral a` in der Typsignatur, obwohl sie im Funktionskörper benötigt wird.

## Mögliche Behebung (Fix)
Füge alle erforderlichen Constraints in der Typsignatur hinzu, etwa durch Ergänzung von `Ord a =>` oder `Integral a =>`, damit die benötigten Operationen zulässig sind.

## English Description
This error occurs when a required type class cannot be deduced from the context for a function or instance. Often, a constraint like `Ord a` or `Integral a` is missing from the type signature, even though it is needed in the function body.

## Suggested Fix
Add all required constraints to the type signature, such as by including `Ord a =>` or `Integral a =>`, to ensure the necessary operations are allowed.


## Regex Pattern
```python
could not deduce.*\(
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
