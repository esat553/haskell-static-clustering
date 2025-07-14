# Überlappende Instanzen (Overlapping Instances)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn für denselben Typ mehrere gleichartige Instanzen definiert wurden, etwa durch gleichzeitiges manuelles Implementieren und automatisches Ableiten (`deriving`). Der Compiler erkennt dadurch mehrere gültige Instanzen für dieselbe Klasse und denselben Typ, was zu einem Konflikt führt.

## Mögliche Behebung (Fix)
Vermeide doppelte Instanzdefinitionen. Entweder eine Instanz selbst definieren oder sie automatisch mit `deriving` erzeugen – nicht beides gleichzeitig. Falls `deriving` verwendet wird, darf keine eigene gleichartige Instanz mehr vorhanden sein.

## English Description
This error occurs when multiple instances of the same type class are defined for the same type, for example by combining a manual implementation with a `deriving` clause. The compiler finds overlapping definitions and raises a conflict.

## Suggested Fix
Avoid duplicate instance declarations. Either define the instance manually or derive it automatically, but not both. If you use `deriving`, do not write a custom instance of the same type class.


## Regex Pattern
```python
overlapping instances for
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
