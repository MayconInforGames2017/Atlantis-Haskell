module Lib where 

type Pos = (Float, Float)
 
data Direcao = Leste | Oeste | Parado deriving (Eq, Show)

data Modo = Inicio | Jogando | GameOver deriving (Eq, Show)

data Mundo = Estado {
  naveEsquerda :: Pos
, naveMeio :: Pos
, naveDireita :: Pos   
, maxPontos :: Int
, pontos :: Int
} deriving Show

mundoInicial :: Mundo
mundoInicial = Estado {
    naveEsquerda = (0,0, - 50)
  , naveMeio = (10,0, -200)
  , naveDireita = (100, -250)
  , maxPontos = 99
  , pontos = 22
}

linhas :: Num a => a
linhas = 39

limite :: (Num a, Integral a) => a
limite = linhas `div` 2

tamJanela :: Num a => a
tamJanela = tamSegmt * linhas

tamSegmt :: Num a => a
tamSegmt = 10

acaoFrames :: Num a => a
acaoFrames = 10

desenhaSegmento :: Pos -> Picture
desenhaSegmento (x,y) = translate (fromIntegral x * tamSegmt) (fromIntegral y * tamSegmt) $ rectangleSolid tamSegmt tamSegmt

desenhaMundo :: Mundo -> Picture
desenhaMundo m = pictures $ sequenceA [naveNorte (0, 50)(-50, 0) (50, 0), torreNorte] m 

naveNorte :: Point -> Point -> Point -> Picture 
naveNorte p1 p2 p3 = Polygon [p1, p2, p3]

torreNorte :: Float -> Float -> Picture 
torreNorte sizeX sizeY 
  = Polygon $ rectanglePath sizeX sizeY

tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyUp) Down _ _) est@(Estado (x,y) _ _) = est {nave = (x,y + 1)}
tratarEvento (EventKey (SpecialKey KeyDown) Down _ _) est@(Estado (x,y) _ _) = est {nave = (x,y - 1)}
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) est@(Estado (x,y) _ _) = est {nave = (x - 1,y)}
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) est@(Estado (x,y) _ _) = est {nave = (x + 1,y)}
tratarEvento _ est = est

atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo t est = est