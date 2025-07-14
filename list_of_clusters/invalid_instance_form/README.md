# Ungültige Instanz-Form (Invalid Instance Form)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Instanzdeklaration keine gültige Form hat. In Haskell müssen alle Instanztypen der Form `(T a1 ... an)` entsprechen, wobei `T` ein konkreter Typkonstruktor ist und `a1 ... an` verschiedene Typvariablen sind. Instanzen wie `Monoid Int` oder `Monoid (a -> a)` sind ohne Sprachoptionen nicht zulässig.

## Mögliche Behebung (Fix)
Stelle sicher, dass der Instanzkopf eine gültige Form besitzt, z. B. `Monoid (MyType a)`. Vermeide konkrete Typen oder Funktionstypen im Instanzkopf, außer du aktivierst Sprachoptionen wie `FlexibleInstances`.

## English Description
This error occurs when the instance head does not follow the required shape. Haskell expects all instance heads to have the form `(T a1 ... an)`, where `T` is a concrete type constructor and the `a`s are distinct type variables. Instances like `Monoid Int` or `Monoid (a -> a)` are not allowed unless language extensions are enabled.

## Suggested Fix
Make sure the instance head uses a valid type form, such as `Monoid (MyType a)`. Avoid using concrete types or function types unless the `FlexibleInstances` extension is enabled.



## Regex Pattern
```python
illegal instance declaration|Use FlexibleInstances
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
