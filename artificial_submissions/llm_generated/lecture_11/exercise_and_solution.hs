{-
Definiere einen parametrischen Datentyp Box a = Full a | Empty und implementiere dafür Instanzen der Typklassen Functor und Foldable. Zeige mit fmap und foldMap, wie der Inhalt verändert bzw. ausgewertet werden kann.
-}
{-
english translation:
Define a parametric data type Box a = Full a | Empty and implement instances of the type classes Functor and Foldable for it. Demonstrate how the content can be transformed with fmap and evaluated with foldMap.
-}
-- in GHC versions above 9 the Sum module must be imported
import Data.Monoid (Sum(..))
data Box a = Full a | Empty deriving (Show)

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f (Full x) = Full (f x)
  fmap _ Empty    = Empty

instance Foldable Box where
  foldMap :: Monoid m => (a -> m) -> Box a -> m
  foldMap f (Full x) = f x
  foldMap _ Empty    = mempty

main :: IO ()
main = do
  let myBox = Full 10
  let emptyBox = Empty :: Box Int

  let modifiedBox = fmap (+5) myBox
  putStrLn $ "fmap (+5) (Full 10) = " ++ show modifiedBox

  let sumBox = foldMap (\x -> Sum x) myBox
  putStrLn $ "foldMap (\\x -> Sum x) (Full 10) = " ++ show sumBox

  let sumEmptyBox = foldMap (\x -> Sum x) emptyBox
  putStrLn $ "foldMap (\\x -> Sum x) Empty = " ++ show sumEmptyBox