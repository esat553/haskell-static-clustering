{-
Definierens Sie eine geeignete Instanz für Semigroup und Monid: Einen Datentyp fur komplexe Zahlen, ¨ ComplexNumber, mit einem Konstruktor C, der ein Paar von Double als Parameter nimmt. Implementieren Sie dann die Multiplikation zweier komplexer Zahlen als Monoid. Definieren Sie zus¨atzlich eine Instanz fur der Typklasse ¨ Show, welche die komplexe Zahl in der Form a + bi ausgibt, wobei a der Realteil und b der Imagin¨arteil der Zahl ist.
-}
{-
english translation:
Define a suitable instance for Semigroup and Monoid:
Create a data type for complex numbers, ComplexNumber, with a constructor C that takes a pair of Double values as its parameter.
Then implement the multiplication of two complex numbers as the monoid operation.
Additionally, define an instance of the Show type class that displays the complex number in the form a + bi, where a is the real part and b is the imaginary part of the number.
-}

data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)
