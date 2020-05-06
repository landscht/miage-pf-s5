
import Graphics.Gloss
import Data.List

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

-- 1)

motSuivant :: Regles -> Mot -> Mot
motSuivant _ [] = []
motSuivant r (x:xs) = r x ++ motSuivant r xs

motSuivant' :: Regles -> Mot -> Mot
motSuivant' r x = concat (map r x)

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r x = concat [r s | s <- x]

-- 2)

reglesFlocon :: Symbole -> Mot
reglesFlocon 'F' = "F-F++F-F"
reglesFlocon '+' = "+"
reglesFlocon '-' = "-"

-- 3)

lsystem :: Axiome -> Regles -> LSysteme
lsystem a r = a : lsystem (motSuivant r a) r

-- 4)

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

etatInitial :: Config -> EtatTortue
etatInitial (etat,_,_,_,_) = etat

longueurPas :: Config -> Float
longueurPas (_,long,_,_,_) = long

facteurEchelle :: Config -> Float
facteurEchelle (_,_,fact,_,_) = fact

angle :: Config -> Float
angle (_,_,_,ang,_) = ang

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,symb) = symb

-- 5)

avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),cap) = ( ( x + longueurPas c * cos cap , y + longueurPas c * sin cap) , cap )

-- 6)

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c (p,cap) = ( p , cap + (angle c) )

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c (p,cap) = ( p , cap - (angle c) )

-- 7)

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue c (x:xs) | x `elem` symbolesTortue c = x : filtreSymbolesTortue c xs
                              | otherwise = filtreSymbolesTortue c xs
-- 8)

type EtatDessin = (EtatTortue, Path)

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole c (etatTortue, p) 'F' = (newEtat, p ++ [fst newEtat])
                 where newEtat = avance c etatTortue
interpreteSymbole c (etatTortue, p) '+' = (newEtat, p ++ [fst newEtat])
                 where newEtat = tourneAGauche c etatTortue
interpreteSymbole c (etatTortue, p) '-' = (newEtat, p ++ [fst newEtat])
                 where newEtat = tourneADroite c etatTortue
-- 9)
--Nous avons ajouté le point en tête car cela va avoir un conséquence sur la complexité. En effet, nous n'avons pas à parcourir toute la liste contrairement à l'ajout en queue qui est plus couteux. Par ailleurs, c'est tout aussi vraie pour la question13 lorsque l'on fait une sauvegarde des états tortues.
-- 10)
interpreteMot :: Config -> Mot -> Picture
interpreteMot c m = line (fst (etatInitial c) : snd etatDessin)
        where etatDessin = intermediaire  (etatInitial c,[fst(etatInitial c)]) (filtreSymbolesTortue c m)
              intermediaire etatDessin [] = etatDessin
              intermediaire etatDessin (x:xs) = intermediaire newEtatDessin xs
                where newEtatDessin = interpreteSymbole c etatDessin x

--- DRAGON ---
dragon :: LSysteme
dragon = lsystem "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

dragonConfig :: Config
dragonConfig = (((0, 0), 0), 50, 1, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon dragonConfig

--- VON KOCH ---

vonKoch1 :: LSysteme
vonKoch1 = lsystem "F" regles
    where regles 'F' = "F+F--F+F"
          regles  s  = [s]

vKConfig :: Config
vKConfig = (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 vKConfig

--- VON KOCH 2
vonKoch2 :: LSysteme
vonKoch2 = lsystem "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vkConfig2 :: Config
vkConfig2 = (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 vkConfig2

--- HILBERT
hilbert :: LSysteme
hilbert = lsystem "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

hilbertConfig :: Config
hilbertConfig = (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert hilbertConfig

-- 11)

--- MAIN ---
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white vonKoch1Anime

lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime l (etat,long,fact,ang,symb) f = (interpreteMot  (etat,long*(fact^enieme),fact,ang,symb) <$> l) !! enieme
      where enieme = (round f `mod` 6)
