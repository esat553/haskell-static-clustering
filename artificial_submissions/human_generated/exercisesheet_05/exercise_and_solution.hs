{-
Implementieren Sie einen neuen Datentyp Player, um einen professionellen Fußballspieler zu modellieren. Um dies vernunftig zu gestalten, beschränken Sie Ihre Definition auf die Erfullung der folgenden Anforderungen. Verwenden Sie denselben Bezeichner sowohl fur den Typ als auch für seinen Konstruktor.
-}
{-
Implement a new data type Player to model a professional soccer player. To keep this reasonable, restrict your definition to fulfilling the following requirements. Use the same name for both the type and its constructor.
-}
data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 LeftF Forward