module QR.Types where

-- L...7%, M...15%, Q...25%, H...30%
data ErrorCorrectionLevel = LEVEL_L | LEVEL_M | LEVEL_Q | LEVEL_H deriving (Eq,Show)

data Coding_Mode = OnlyNum | Num_ALPHA | EightBitByte | Kanji deriving (Eq, Show)

data Version = Auto
    | Version1 | Version2 | Version3 | Version4 | Version5
    | Version6 | Version7 | Version8 | Version9 | Version10
    | Version11 | Version12 | Version13 | Version14 | Versio15
    | Version16 | Version17 | Version18 | Version19 | Version20
    | Version21 | Version22 | Version23 | Version24 | Version25
    | Version26 | Version27 | Version28 | Version29 | Version30
    | Version31 | Version32 | Version33 | Version34 | Version35
    | Version36 | Version37 | Version38 | Version39 | Version40
    deriving (Eq, Show, Ord, Enum)

data QRCode = QRCode QRWidth [Binary] deriving (Eq, Show)

data Binary = I | O deriving (Eq, Show)

type QRWidth = Int