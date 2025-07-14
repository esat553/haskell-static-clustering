# Falsche Anzahl von Typ-Argumenten (Wrong Number of Type Arguments)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Typparameter fehlt oder ein Typ an einer Stelle verwendet wird, an der ein Typkonstruktor mit weiteren Parametern erwartet wird. Oft passiert das bei Instanzdeklarationen oder Datendefinitionen, bei denen ein parametrischer Typ nicht korrekt angewendet wurde.

## Mögliche Behebung (Fix)
Überprüfe, ob alle Typargumente korrekt angegeben wurden. Verwende Typen wie `BB a` statt nur `BB` und achte darauf, dass Konstruktoren und Instanzen mit vollständigen Typen arbeiten.

## English Description
This error occurs when a type constructor is used without the required number of type arguments. It typically happens in instance declarations or type definitions, where a higher-kinded type is used as if it were fully applied.

## Suggested Fix
Ensure all required type parameters are supplied. For example, use `BB a` instead of just `BB`, and check that instance heads refer to fully applied types.

## Regex Pattern
```python
expecting one more argument to .*has kind
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
