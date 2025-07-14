# Lexikalischer Fehler (Lexical Error)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im Quelltext ein Zeichen verwendet wird, das von Haskell bzw. dem GHC-Compiler nicht als gültiges Symbol erkannt wird. Typisch ist dies bei der Verwendung von typografischen Anführungszeichen oder Apostrophen (z. B. `’` statt `'`), nicht-lateinischen oder Steuerzeichen.

## Mögliche Behebung (Fix)
Ersetze alle ungültigen oder typografischen Zeichen im Quelltext durch die in Haskell zulässigen ASCII-Zeichen, insbesondere Standard-Apostrophen und Anführungszeichen.

## English Description
This error occurs when a character appears in the source code that is not recognized as valid by Haskell or the GHC compiler. Common cases include typographic quotes or apostrophes (e.g., `’` instead of `'`), non-Latin characters, or control characters.

## Suggested Fix
Replace all invalid or typographic characters in the source code with valid ASCII characters accepted by Haskell, especially standard apostrophes and quotation marks.


## Regex Pattern
```python
lexical error at character
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
