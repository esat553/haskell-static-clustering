# Ungültiges Enum-Deriving (Invalid Enum Deriving)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn für einen Datentyp das automatische Ableiten (`deriving`) der Typklasse `Enum` versucht wird, obwohl der Typ kein gültiger Aufzählungstyp ist. Ein Enumerationstyp muss ausschließlich aus nullären Konstruktoren bestehen, also Konstruktoren ohne Argumente.

## Mögliche Behebung (Fix)
Definiere den Datentyp als reine Aufzählung mit nur nullären Konstruktoren, wenn `Enum` abgeleitet werden soll, oder entferne das `deriving (Enum)` aus der Deklaration.

## English Description
This error occurs when trying to automatically derive the `Enum` type class for a data type that is not a valid enumeration. An enumeration type must consist only of nullary constructors, i.e., constructors without arguments.

## Suggested Fix
Define the data type as a pure enumeration with only nullary constructors to derive `Enum`, or remove `deriving (Enum)` from the declaration.


## Regex Pattern
```python
can't make a derived instance of ['‘`]Enum
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
