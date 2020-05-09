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
data Element = Na | Cl | Fe | Mg | Au | Ag | Bi | H | S deriving (Eq,Show,Enum)

data Structure = HCP | CCP | FCC | BCC deriving (Eq)

--原子の配置は，頂点で共有 > 稜で共有 > 面で共有 の順に安定
data Polyhedron = Vertex | Ridge | Surface deriving (Eq, Show, Enum, Ord)

-- 岩塩型，閃亜鉛鉱型，ウルツ鉱型，塩化セシウム型
data Madelung = NaCl | ZnS | UZnS | CsCl deriving (Eq, Show)

instance Show Structure where
    show HCP = "hexagonal close-packed structure"
    show CCP = "cubic close-packed structure"
    show FCC = "facxe-centered cubic structure"
    show BCC = "body-centered cubic structure"