{-# LANGUAGE BinaryLiterals #-}

module QR.Image where

import QR.Types
import Data.Char (ord)
import Data.List (reverse)
import Data.Bits
import Data.Bits.Extras (w8, w32)
import Data.Word
import Data.ByteString as B (ByteString, singleton, pack, writeFile)

pngSignature = map w8 $
    [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

iHDR :: Int -> [Word8]
iHDR width = 
    map w8 [0x00,0x00,0x00,0x13]
    ++ chunkType
    ++ chunkData
    ++ getCRC (chunkType ++ chunkData)
    where
        chunkType = map (w8.ord) "IHDR" -- [0x49,0x48,0x44,0x52]
        chunkData = map w8 $
            [0x00, 0x00, 0x00, width] -- width
            ++ [0x00, 0x00, 0x00, width] -- height
            ++ [8] -- bit depth
            ++ [0] -- color type (monotone)
            ++ [0] -- compress
            ++ [0] -- filter
            ++ [0] -- interrace

iDAT :: QRCode -> [Word8]
iDAT (QRCode w cs) =
    map w8 [0x00,0x00,0x00,w] -- length
    ++ chunkType
    ++ chunkData
    ++ getCRC (chunkType ++ chunkData)
    where
        chunkType = map (w8.ord) "IDAT" -- [0x49, 0x44, 0x41, 0x54]
        chunkData = map (w8.coloring) cs
        coloring I = 0x00
        coloring O = 0xff

iEND :: [Word8]
iEND =
    map w8 [0x00]
    ++ chunkType
    ++ getCRC chunkType -- [0xae, 0x42, 0x60, 0x82]
    where
        chunkType = map (w8.ord) "IEND" -- [0x49, 0x45, 0x4e, 0x44]

getCRC :: [Word8] -> [Word8]
getCRC dat = toWord8s .fromIntegral. complement $ getCRC' (w32 0xffffffff) dat
    where
        getCRC' crc [] = crc
        getCRC' crc (x:xs) = getCRC' (updateCRC crc x) xs
        pngMagicNum = w32 0xedb88320 -- reversed
        updateCRC :: Word32 -> Word8 -> Word32
        updateCRC crc byte = foldl (f byte) crc [0..7]
        f :: Word8 -> Word32 -> Int -> Word32
        f byte crc i = if testBit crc' 0
            then xor pngMagicNum $ shift crc' (-1)
            else shift crc' (-1)
            where
                crc' = if testBit (shift byte (-i)) 0 then complementBit crc 0 else crc

toWord8s :: Int -> [Word8]
toWord8s = reverse . toWord8s'
    where
        toWord8s' x
            | x <= 0xff = [w8 x]
            | otherwise = w8 (mod x 256) : toWord8s' (shift x (-8))

writePng :: FilePath -> QRCode -> IO ()
writePng file = B.writeFile file . generatePng

generatePng :: QRCode -> ByteString
generatePng qr@(QRCode w _) = pack $
    pngSignature
    ++ iHDR w
    ++ iDAT qr
    ++ iEND