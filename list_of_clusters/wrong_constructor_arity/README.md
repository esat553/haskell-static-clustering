# Falsche Arität für Konstruktor (Wrong Constructor Arity)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Datentyp-Konstruktor mit einer falschen Anzahl an Argumenten verwendet wird. Konstruktoren erwarten eine fest definierte Anzahl von Parametern. Wird diese Anzahl beim Pattern-Matching oder bei der Anwendung nicht eingehalten, meldet der Compiler eine Aritätsverletzung.

## Mögliche Behebung (Fix)
Es ist sicherzustellen, dass der Konstruktor mit exakt der Anzahl von Argumenten verwendet wird, wie sie in der zugehörigen `data`-Definition angegeben ist.

## English Description
This error occurs when a data constructor is used with an incorrect number of arguments. Constructors expect a fixed number of parameters. If the actual usage provides too few or too many arguments, the compiler reports an arity mismatch.

## Suggested Fix
Ensure that the constructor is used with exactly the number of arguments defined in its `data` declaration. For example, a constructor must be applied to a single tuple, not to three separate values.

## Regex Pattern
```python
the constructor .* should have \d+ argument[s]?, but has been given \d+
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
