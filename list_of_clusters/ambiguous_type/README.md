# Mehrdeutiger Typ (Ambiguous Type)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn Haskell einen Typ nicht eindeutig bestimmen kann, etwa bei Literalen ohne Kontext. Häufig fehlt eine Typannotation, sodass unklar bleibt, welche Instanz einer Typklasse wie `Num` verwendet werden soll.

## Mögliche Behebung (Fix)
Füge eine eindeutige Typannotation hinzu, z. B. `:: Int` oder `:: [Int]`, um den gewünschten Typ explizit anzugeben.

## English Description
This error occurs when Haskell cannot resolve a type variable due to missing context, often involving literals. Without a type annotation, the required type class instance (e.g. `Num`) remains ambiguous.

## Suggested Fix
Add a type annotation such as `:: Int` or `:: [Int]` to make the intended type explicit.

## Regex Pattern
```python
ambiguous type variable
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
