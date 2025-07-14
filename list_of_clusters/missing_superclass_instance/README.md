# Fehlende Superklassen-Instanz (Missing Superclass Instance)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Instanz für eine Typklasse deklariert wird, ohne dass alle ihre Superklassen bereits instanziiert sind. Wer einen `Monoid` instanziieren will, muss auch eine gültige Instanz für `Semigroup` bereitstellen.

## Mögliche Behebung (Fix)
Füge zuerst eine gültige `Semigroup`-Instanz hinzu. Die `(<>)`-Funktion muss korrekt definiert werden, bevor eine `Monoid`-Instanz erstellt werden kann. Nur dann erkennt der Compiler die `Monoid`-Instanz als gültig an.

## English Description
This error occurs when an instance of a type class is declared without also declaring all required superclasses. If you want to define a `Monoid` instance, a valid `Semigroup` instance must exist first.

## Suggested Fix
Add a valid `Semigroup` instance first. The `(<>)` function must be defined properly before creating the `Monoid` instance. Only then will the compiler accept the `Monoid` instance.


## Regex Pattern
```python
no\s+instance\s+for.*arising\s+from\s+the\s+superclasses
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
