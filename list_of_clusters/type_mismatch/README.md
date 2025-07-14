# Typenkonflikt (Type Mismatch)

## Deutsche Beschreibung
Dieser allgemeine Cluster fasst fundamentale Typfehler zusammen, bei denen ein Wert eines bestimmten Typs an einer Stelle verwendet wird, wo ein anderer Typ erwartet wird. Häufige Ursachen sind die falsche Vermischung von `IO`-Aktionen und reinen Werten (z.B. durch falschen Einsatz von `<-` statt `let`), die Rückgabe eines Wertes, der nicht zur Typ-Signatur passt, oder der Aufruf einer Funktion mit einer falschen Anzahl an Argumenten.

## Mögliche Behebung (Fix)
Stelle sicher, dass Rückgabewerte zur Signatur passen und Funktionen mit der korrekten Anzahl an Argumenten aufgerufen werden.

## English Description
This general cluster covers fundamental type errors where a value of one type is used in a context that expects a different type. Common causes include incorrectly mixing `IO` actions and pure values (e.g., misusing `<-` instead of `let`), returning a value that does not match the type signature, or calling a function with the wrong number of arguments.

## Suggested Fix
Verify that functions are called with the correct number of arguments.

## Regex Pattern
```python
couldn't? match (expected type|type)
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
