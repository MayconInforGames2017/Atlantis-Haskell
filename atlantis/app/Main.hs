module Main where

import Lib
import Graphics.Gloss
import System.Random
import UI

main :: IO ()
main = do
    let mundo = mundoInicial
    stdGen <- getStdGen
    play (InWindow "hAtlantis" (900, 900)(3000, 100)) black 60 mundo desenhaMundo tratarEvento atualizaMundo

