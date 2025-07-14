# Fehlende GADTs-Erweiterung (Missing GADTs Extension)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine allgemeine algebraische Datentyp-Deklaration (GADT) verwendet wird, ohne dass die Sprachoption `GADTs` aktiviert ist. Haskell verlangt in diesem Fall explizit das Sprach-Feature, um solche Datentypen zuzulassen.

## Mögliche Behebung (Fix)
Aktiviere die GADTs-Erweiterung, indem du `{-# LANGUAGE GADTs #-}` an den Anfang der Datei schreibst.

## English Description
This error occurs when a generalised algebraic data type (GADT) declaration is used without enabling the `GADTs` language extension. Haskell requires this extension to allow such data type definitions.

## Suggested Fix
Enable the GADTs extension by adding `{-# LANGUAGE GADTs #-}` at the top of the file.

## Regex Pattern
```python
enable the GADTs extension
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
