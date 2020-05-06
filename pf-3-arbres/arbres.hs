import Test.QuickCheck()
import Control.Concurrent (threadDelay)

main' :: IO ()
main' = undefined

-- 1)

data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val)
                    | Feuille
                    deriving Show
-- 2)

mapArbre :: (c1 -> v1 -> c2) -> (c1 -> v1 -> v2) -> Arbre c1 v1 -> Arbre c2 v2
mapArbre fc fv  = foldArbre(\ c1 v1 nfg nfd -> Noeud (fc c1 v1) (fv c1 v1) nfg nfd) Feuille

-- 3)
hauteur :: (Arbre c v) -> Int
hauteur (Noeud _ _ a1 a2) = 1 + max (hauteur a1) (hauteur a2)
hauteur Feuille = 0

hauteur' :: (Arbre c v) -> Int
hauteur' = foldArbre(\ _ _ fg fd -> 1 + fg `max` fd) 0

taille :: (Arbre c v) -> Int
taille (Noeud _ _ a1 a2) = 1 + hauteur a1 + hauteur a2
taille Feuille = 0

taille' :: (Arbre c v) -> Int
taille' = foldArbre (\ _ _ fg fd -> 1 + fg + fd) 0
-- 4)
foldArbre :: (coul -> val -> a -> a -> a) -> a -> Arbre coul val -> a
foldArbre noeud feuille Feuille = feuille
foldArbre noeud feuille (Noeud coul val fg fd) = noeud coul val (foldArbre noeud feuille fg) (foldArbre noeud feuille fd)

-- 5)
peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((coul,val):xs) = Noeud coul val (peigneGauche xs) Feuille

-- 6)
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)
prop_hauteurPeigne' xs = length xs == hauteur' (peigneGauche xs)
-- 7)
-- Pour un peigne, la taille est égale à la longueur de l'arbre et donc à la hauteur vu qu'il y a une feuille pour chaque fils droit
prop_taillePeigne xs = length xs == taille(peigneGauche xs)
prop_taillePeigne' xs = length xs == taille' (peigneGauche xs)

-- 8)
estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet (Noeud c v a1 a2) = estComplet a1 && estComplet a2 && (hauteur a1 == hauteur a2)

-- 9)
--estComplet' :: Arbre c a -> Bool
--estComplet' = foldArbre (\ _ _ fg fd -> ) True

-- 10)
-- arbre de hauteur 0 ou 1

-- 11)
complet :: Int -> [(c, a)] -> Arbre c a
complet h _ | h<=0 = Feuille
complet _ [] = Feuille
complet h l = Noeud c v
                    (complet h' lg)
                    (complet h' ld)
            where
              (lg,(c,v):ld) = splitAt (2^h'-1) l
              h'=h-1

complet' :: Int -> [(c, a)] -> Arbre c a
complet' h _ | h<=0 = Feuille
complet' _ [] = Feuille
complet' h l = fst (completAux h l)
  where completAux _ [] = (Feuille,[])
        completAux 0 l = (Feuille, l)
        completAux h l = (Noeud c v fg fd,ld)
          where
            (fg,(c,v):lg)=completAux h' l
            (fd,ld)=completAux h' lg
            h'=h-1

prop_complet = True == estComplet (complet 2 [((),'a'),((),'b'),((),'c')])

-- to do some tests

-- 12

infini :: a -> [a]
infini a = a : infini a

infini' :: a -> [a]
infini' a = concat(iterate (++ [a])[a])

-- 13

createListe :: [a] -> [((),a)]
createListe [] = []
createListe (x:xs) = ((),x) : createListe xs

createListe' :: [a] -> [((),a)]
createListe' l = foldr (\e res -> ((),e) : res) [] l

-- 14
aplatit :: Arbre c a -> [(c,a)]
aplatit  = foldArbre(\ coul val fg fd ->  fg ++ (coul,val):fd ) []

test_aplatit  = map snd (aplatit (complet 4 (createListe' ['a'..'o']))) == "abcdefghijklmno"

-- 15
element :: Eq a => a -> Arbre c a -> Bool
element v Feuille = False
element v (Noeud coul val fg fd) = (val == v) || element v fg || element v fd

-- 16

noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud colorToString valueToString (coul,val) = (valueToString val) ++ "[color=" ++ (colorToString coul) ++ ", fontcolor=" ++ (colorToString coul) ++ "]"

--17

arcs :: Arbre c a -> [(a,a)]
arcs Feuille = []
arcs (Noeud co vo Feuille Feuille) = []
arcs (Noeud c v (Noeud cg vg fgg fgd) (Noeud cd vd fdg fdd)) = arcs (Noeud cg vg fgg fgd) ++ ((v, vg) : (v, vd) : arcs (Noeud cd vd fdg fdd))
arcs (Noeud c v (Noeud cg vg fgg fgd) Feuille) = [(v,vg)] ++ arcs (Noeud cg vg fgg fgd)
arcs (Noeud c v Feuille (Noeud cd vd fdg fdd) ) = [(v,vd)] ++ arcs(Noeud cd vd fdg fdd)

-- 18

arc :: (a -> String) -> (a,a) -> String
arc valueToString (val1,val2) = (valueToString val1) ++ " -> " ++ (valueToString val2)

-- 19
valueToString :: Char -> String
valueToString c = [c]

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise name f g arbre = unlines (
                    ["digraph \"" ++ name ++ "\" {",
                     "node [shape=circle]"]
                    ++ (map (noeud f g) (aplatit arbre))
                    ++ (map (arc g) (arcs arbre))
                    ++ ["}"]
                   )

testArbre :: IO ()
testArbre = do
        writeFile "test.dot" (dotise "test" (\_ -> "red") valueToString (complet 4 (createListe ['a'..'o'])))

-- 20)

elementR :: (Eq a, Ord a) => a -> Arbre c a -> Bool
elementR v Feuille = False
elementR v (Noeud coul val fg fd) | (val == v) = True
                                 | v > val = element v fd
                                 | otherwise = element v fg

-- 21)
data Couleur = R | N
     deriving (Show,Eq)

colorToString :: Couleur -> String
colorToString R = "red"
colorToString _ = "black"


type ArbreRN a = Arbre Couleur a
-- 22)
equilibre :: ArbreRN a -> ArbreRN a
equilibre Feuille = Feuille
equilibre (Noeud N z (Noeud R y (Noeud R x a b ) c ) d)  = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N z (Noeud R x a (Noeud R y b c)) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N x a (Noeud R z (Noeud R y b c) d ) ) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N x a (Noeud R y b (Noeud R z c d ) ) ) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre other = other

-- 23
insertionArbre :: (Eq a, Ord a) => a -> Arbre Couleur a -> Arbre Couleur a
insertionArbre val Feuille = Noeud N val Feuille Feuille
insertionArbre val (Noeud c v Feuille fd) | val < v = Noeud c v (Noeud R val Feuille Feuille ) fd
insertionArbre val (Noeud c v fg Feuille) | val > v = Noeud c v fg (Noeud R val Feuille Feuille)
insertionArbre val (Noeud c v fg fd) | val < v = equilibre (Noeud c v (insertionArbre val fg) fd )
                                     | val > v = equilibre (Noeud c v fd (insertionArbre val fd) )
                                     | otherwise = (Noeud c v fg fd)

-- 24

-- 25

-- 26
intermediaire :: (Eq a, Ord a) => [a] -> Arbre Couleur a -> [Arbre Couleur a]
intermediaire [] a = [a]
intermediaire (e:l) a = a : (intermediaire l (insertionArbre e a))


arbresDot :: [Char] -> [String]
arbresDot s = map (dotise "ABR" colorToString (\x -> [x])) (intermediaire s Feuille)

main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"
