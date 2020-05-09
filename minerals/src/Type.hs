module Type where

type ValenceE = Int
type Coordinate = Int
type Radius = Float
type Distance = Float

data Pole = Plus | Minus deriving (Eq,Show)
data Ion = Ion {
    pole :: Pole,
    elem :: Element,
    eN :: ValenceE,
    radius :: Radius} deriving (Eq,Show)

data Mineral = Ionic [Ion] | Covalent [Element] | Vanderwaals [Element] deriving (Eq, Show)

--元素
data Element = Na | Cl | Mg | Au deriving (Eq,Show)

data Structure = HCP | CCP | FCC | BCC deriving (Eq, Show)

data Polyhedron = Vertex | Ridge | Surface deriving (Eq, Show, Enum, Ord)