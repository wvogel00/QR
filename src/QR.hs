module QR where

import QR.Types
import Data.List (lookup)
import Data.Maybe (fromJust)
import Data.Char (ord)

generator :: ErrorCorrectionLevel -> Coding_Mode -> Version -> String -> QRCode
generator lv mode Auto str = generator lv mode (decideVersion lv mode) str
generator lv mode ver str = QRCode (getWidth ver) $ [I,I,O]

decideVersion :: ErrorCorrectionLevel -> Coding_Mode -> Version
decideVersion lv mode = undefined

maxCharacter :: Version -> Coding_Mode -> ErrorCorrectionLevel -> Int
maxCharacter Version1 OnlyNum LEVEL_L = 41
maxCharacter Version1 OnlyNum LEVEL_M = 34
maxCharacter Version1 OnlyNum LEVEL_Q = 27
maxCharacter Version1 OnlyNum LEVEL_H = 17
maxCharacter Version1 Num_ALPHA LEVEL_L = 25
maxCharacter Version1 Num_ALPHA LEVEL_M = 20
maxCharacter Version1 Num_ALPHA LEVEL_Q = 16
maxCharacter Version1 Num_ALPHA LEVEL_H = 10
maxCharacter Version1 EightBitByte LEVEL_L = 17
maxCharacter Version1 EightBitByte LEVEL_M = 14
maxCharacter Version1 EightBitByte LEVEL_Q = 11
maxCharacter Version1 EightBitByte LEVEL_H = 7
maxCharacter Version1 Kanji LEVEL_L = 10
maxCharacter Version1 Kanji LEVEL_M = 8
maxCharacter Version1 Kanji LEVEL_Q = 7
maxCharacter Version1 Kanji LEVEL_H = 4
maxCharacter _ _ _ = undefined

getWidth :: Version -> Int
getWidth ver = fromJust . lookup ver $ zip [Version1 ..] [21,25..177]

modeCode :: Coding_Mode -> [Binary]
modeCode OnlyNum = [O,I,I,I]
modeCode Num_ALPHA = [O,O,O,I]
modeCode EightBitByte = [O,O,I,O]
modeCode Kanji = [I,O,O,O]

encode :: Coding_Mode -> String -> [Binary]
encode mode str = modeCode mode ++ lengthCode str ++ encodeStr mode str

lengthCode :: String -> [Binary]
lengthCode = toBin . length

encodeStr :: Coding_Mode -> String -> [Binary]
encodeStr mode str = case mode of
    OnlyNum   -> encodeNum str
    Num_ALPHA -> encodeStr_NumALPHA str
    EightBitByte -> encode8bitStr str
    Kanji -> encodeKanji str

encodeNum str = undefined
encode8bitStr str = undefined
encodeKanji str = undefined

encodeStr_NumALPHA [] = []
encodeStr_NumALPHA [c] = adjustLen 4 $ toBin $ ord c
encodeStr_NumALPHA (a:b:str) = toBin (45*ord a + ord b) ++ encodeStr_NumALPHA str

toBin :: Int -> [Binary]
toBin 0 = []
toBin v = reverse $ toBin' v where
    toBin' 0 = []
    toBin' v = (if mod v 2 == 1 then I else O) : toBin' (div v 2)

saveQRPNG :: FilePath -> QRCode -> IO ()
saveQRPNG file qr = return ()