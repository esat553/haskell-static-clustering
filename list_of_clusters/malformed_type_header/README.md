# Fehlerhafter Typ-Header (Malformed Type Header)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn der Kopf einer `data`- oder `class`-Deklaration nicht dem erwarteten Format entspricht. Häufige Ursachen sind Kleinschreibung von Typnamen, ungültige Zeichen oder Syntaxfehler beim Namen des Typs oder der Klasse.

## Mögliche Behebung (Fix)
Stelle sicher, dass Typ- und Klassennamen mit einem Großbuchstaben beginnen und keine ungültigen Zeichen enthalten. Kontrolliere die Syntax der Deklaration sorgfältig.

## English Description
This error occurs when the head of a `data` or `class` declaration does not follow the expected format. Common causes are using lowercase for type names, invalid characters, or syntax errors in the type or class name.

## Suggested Fix
Ensure that type and class names start with a capital letter and do not contain invalid characters. Check the declaration syntax carefully.

## Regex Pattern
```python
malformed head of type or class declaration
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
