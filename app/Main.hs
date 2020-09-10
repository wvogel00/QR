module Main where

import QR
import QR.Types

main :: IO ()
main = do
    putStrLn "QR code generator & reader application"
    let qr = generator LEVEL_L EightBitByte Version1 "haskell lover"
    saveQRPNG "haskellQR.png" qr