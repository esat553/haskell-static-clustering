# Typenkonstruktor oder Klasse nicht definiert (Type Constructor or Class Not Defined)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein in der Typsignatur oder im Pattern verwendeter Typ oder Datenkonstruktor nicht im aktuellen Modul definiert oder importiert wurde. Häufig liegt dies daran, dass der entsprechende `data`- oder `newtype`-Block fehlt, der Typ falsch geschrieben ist oder ein notwendiges Modul nicht eingebunden wurde.

## Mögliche Behebung (Fix)
Stelle sicher, dass der verwendete Typ oder Konstruktor korrekt deklariert oder importiert wurde. Wenn der Typ selbst definiert werden soll, muss eine passende `data`- oder `newtype`-Definition vorhanden sein. Andernfalls sollte das fehlende Modul importiert oder der Name korrigiert werden.

## English Description
This error occurs when a type or data constructor used in a type signature or pattern is not defined or imported in the current module. It often results from missing `data` or `newtype` declarations, typos, or omitted imports of required modules.

## Suggested Fix
Ensure that the used type or constructor is either properly declared or imported. If the type is intended to be defined locally, provide a suitable `data` or `newtype` declaration. Otherwise, correct the name or import the missing module.


## Regex Pattern
```python
Not in scope: type constructor or class ‘[A-Z][a-zA-Z0-9_']*’
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
