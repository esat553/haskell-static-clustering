# Flexible Kontexte benötigt (Flexible Contexts Required)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn in einer Typklassen-Constraint kein einfacher Typparameter, sondern ein zusammengesetzter Typ verwendet wird. Standardmäßig erlaubt Haskell solche komplexen Constraints nicht und fordert das Sprach-Feature `FlexibleContexts`.

## Mögliche Behebung (Fix)
Aktiviere `FlexibleContexts` mit `{-# LANGUAGE FlexibleContexts #-}` am Dateianfang oder formuliere die Constraints ausschließlich mit einzelnen Typparametern.

## English Description
This error occurs when a type class constraint contains a composite type instead of a simple type variable. By default, Haskell does not allow such complex constraints and requires the `FlexibleContexts` language extension.

## Suggested Fix
Enable `FlexibleContexts` at the top of the file with `{-# LANGUAGE FlexibleContexts #-}` or rewrite the constraints to use only individual type variables.

## Regex Pattern
```python
non type-variable argument in the constraint
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
