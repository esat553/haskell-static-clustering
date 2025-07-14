# Parse-Fehler durch Import-Fehler (Parse Error due to Import Error)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Student einen Fehler im `import`-Statement macht. Das kann verschiedene Ursachen haben: ein `import` an einer ungültigen Stelle im Code, ein Verweis auf ein nicht existierendes Modul oder ein Syntaxfehler innerhalb der `import`-Anweisung selbst. Solche Fehler führen in der Regel zu einem Parse-Fehler oder einem „module not found“-Fehler.

## Mögliche Behebung (Fix)
Stelle sicher, dass alle `import`-Anweisungen am Anfang des Codes nach dem `module`-Header (falls vorhanden) stehen. Überprüfe Modulnamen auf korrekte Groß- und Kleinschreibung sowie Schreibweise. Verwende nur existierende und im Projekt verfügbare Module.

## English Description
This cluster covers parse errors that occur due to problems in `import` statements. Common causes include placing an `import` at an invalid position in the code, referring to a non-existent module, or using incorrect syntax within the `import` declaration. These issues typically lead to a parse error or a module not found error.

## Suggested Fix
Ensure that all `import` statements appear at the top of the file, after the `module` header if present. Check for correct spelling and casing of module names. Only use modules that exist and are available in the project environment.

## Regex
```python
(could not find module|parse error on input)[\s\S]+?‘?import

```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs) and [example_2.hs](./example_2.hs)
