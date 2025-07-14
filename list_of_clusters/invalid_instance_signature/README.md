# Ungültige Instanz-Signatur (Invalid Instance Signature)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn innerhalb einer `instance`-Deklaration eine Typ-Signatur angegeben wird, ohne dass die Sprachoption `InstanceSigs` aktiviert ist. Standardmäßig erwartet der Compiler in `instance`-Blöcken nur Implementierungen, nicht erneut deklarierte Typ-Signaturen. Die Signatur gilt daher als ungültig.

## Mögliche Behebung (Fix)
Entferne die Typ-Signatur innerhalb der `instance`-Deklaration oder aktiviere explizit die Sprachoption `InstanceSigs`, indem du `{-# LANGUAGE InstanceSigs #-}` am Anfang der Datei einfügst.

## English Description
This error occurs when a type signature is given inside an `instance` declaration without enabling the `InstanceSigs` language extension. By default, the compiler expects only implementations, not re-declared type signatures. As a result, the signature is considered invalid.

## Suggested Fix
Remove the type signature from the `instance` declaration, or explicitly enable the `InstanceSigs` extension by adding `{-# LANGUAGE InstanceSigs #-}` at the top of your file.


For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
