{-
Schreibe eine Funktion cleanVotes :: [String] -> [String], die alle Stimmen entfernt, die kürzer als 4 Buchstaben sind oder exakt "None" heißen – und formuliere sie vollständig im pointfree Style.
-}
{-
english translation:
Write a function cleanVotes :: [String] -> [String] that removes all entries which are shorter than 4 characters or exactly equal to "None" – and formulate it entirely in pointfree style.
-}
import Control.Applicative (liftA2)

cleanVotes :: [String] -> [String]
cleanVotes = filter (liftA2 (&&) ((>= 4) . length) (/= "None"))