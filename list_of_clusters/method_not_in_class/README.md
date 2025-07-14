# Methode nicht in Klasse (Method Not in Class)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn in einer Instanzdefinition eine Methode implementiert wird, die nicht zur angegebenen Typklasse gehört. Das kann passieren, wenn versehentlich eine Instanz für einen Datentyp anstelle einer Typklasse deklariert wird, oder wenn ein falscher Klassenname verwendet wurde. Der Compiler erkennt dann nicht, dass die Methode zu einer gültigen Klasse gehört.

## Mögliche Behebung (Fix)
Überprüfe die `instance`-Deklaration und stelle sicher, dass du eine gültige Typklasse verwendest. Achte darauf, dass du nicht versehentlich eine Instanz für einen Datentyp anstelle einer Typklasse definierst. Verwende nur Methoden, die zur deklarierten Klasse gehören.

## English Description
This error occurs when an instance declaration defines a method that is not part of the specified type class. This can happen when a data type is mistakenly used in place of a type class, or when the wrong class name is given. The compiler then does not recognize the method as belonging to any valid class.

## Suggested Fix
Check the `instance` declaration to ensure a valid type class is used. Make sure you're not trying to create an instance for a data type instead of a type class, and only implement methods that actually belong to the declared class.


## Regex Pattern
```python
is not a \(visible\) method of class
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
