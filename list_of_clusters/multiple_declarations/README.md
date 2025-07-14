# Mehrfache Deklarationen (Multiple Declarations)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Bezeichner (z. B. ein Funktionsname oder ein Datentyp) im selben Gültigkeitsbereich mehr als einmal definiert wird. Dies geschieht typischerweise aus zwei Gründen: Entweder wurde ein Name versehentlich kopiert und wieder eingefügt, oder was noch häufiger vorkommt die verschiedenen Definitionszeilen einer Funktion (Pattern-Matching-Klauseln) wurden durch anderen Code unterbrochen. Haskell behandelt diese getrennten Klauseln dann fälschlicherweise als eine ungültige Neudeklaration der ursprünglichen Funktion.

## Mögliche Behebung (Fix)
Um diesen Fehler zu beheben, suchen Sie nach dem im Fehler genannten Bezeichner und stellen Sie sicher, dass er nur einmal deklariert wird. Falls es sich um eine Funktion mit mehreren Klauseln handelt, ordnen Sie den Code so an, dass alle zusammengehörigen Zeilen einen einzigen, ununterbrochenen Block bilden. Entfernen Sie alle dazwischenliegenden, vollständigen Definitionen oder versehentlich duplizierten Codeblöcke.

## English Description
This error occurs when an identifier (e.g., a function name or a data type) is defined more than once in the same scope. This typically happens for two reasons: either a name was accidentally copied and pasted, or—more commonly—the different definition lines of a function (pattern matching clauses) were interrupted by other code. Haskell then incorrectly treats these separated clauses as an invalid re-declaration of the original function.

## Suggested Fix
To fix this error, search for the identifier mentioned in the error and ensure it is declared only once. If it is a function with multiple clauses, arrange the code so that all related lines form a single, uninterrupted block. Remove any intervening complete definitions or accidentally duplicated code blocks.