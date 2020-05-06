import Parser
import Data.Char
import Data.Maybe
import System.Exit

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

-- Q1

espacesP :: Parser ()
espacesP = (many $ car ' ') >> pure ()

-- Q2

nomP :: Parser Nom
nomP = (some $ carQuand isLetter) >>= \nom -> espacesP >> pure nom

nomP' :: Parser Nom
nomP' = do
  nom <- some $ carQuand isLetter
  espacesP
  return nom

-- Q3
varP :: Parser Expression
varP = Var <$> nomP
-- Q4

applique :: [Expression] -> Expression
applique = foldl1 App

-- Q5
exprP :: Parser Expression
exprP = booleenP <|> nombreP <|> varP <|> lambdaP <|> exprParentheseeP

exprsP :: Parser Expression
exprsP = applique <$> some exprP
-- Q6
lambdaP :: Parser Expression
lambdaP = do
         car '\\'
         espacesP
         x <- nomP
         chaine "->"
         espacesP
         expr <- exprsP
         return (Lam x expr)

-- Q7
-- voir q5

-- Q8
exprParentheseeP :: Parser Expression
exprParentheseeP = do
                  car '('
                  x <- exprsP
                  car ')'
                  espacesP
                  return x

-- Q9
nombreP :: Parser Expression
nombreP = do
          nb <- some (carQuand isNumber)
          espacesP
          return $ Lit $ Entier $ read nb

-- Q10
booleenP :: Parser Expression
booleenP = do
           bool <- chaine "True" <|> chaine "False"
           espacesP
           return $ Lit $ Bool $ read bool

-- Q11
expressionP :: Parser Expression
expressionP = do
              espacesP
              x <- exprsP
              return x

-- Q12
ras :: String -> Expression
ras s = intermediaire (runParser expressionP s)
       where intermediaire Nothing = error "Parser error"
             intermediaire (Just(r,_)) = r

-- Q13/14
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
  show (VFonctionA _) = "λ"
  show (VLitteralA (Entier nb)) = show nb
  show (VLitteralA (Bool bool)) = show bool

type Environnement a = [(Nom, a)]

-- Q15
interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA env (Lit nb) = VLitteralA nb
interpreteA env (Lam nom expr) = VFonctionA (\v -> interpreteA ((nom,v):env)expr)
interpreteA env (Var x) = fromJust $ x `lookup` env
interpreteA env (App expr1 expr2) = case interpreteA env expr1 of
                                     VFonctionA f -> f $ interpreteA env expr2

-- Q16

negA :: ValeurA
negA = VFonctionA f
       where f (VLitteralA (Entier v)) = VLitteralA (Entier (negate v))
             f e = error ("InterpretationSimple error " ++ (show e))

-- Q17

addA :: ValeurA
addA = VFonctionA f
       where f (VLitteralA(Entier v)) = VFonctionA g
                                        where g (VLitteralA(Entier w)) = VLitteralA(Entier(v+w))
                                              g e = error ("InterpretationSimple error "++ (show e))
             f e = error ("InterpretationSimple error "++ (show e))  

-- Q18                         

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if", ifthenelseA) ]

releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA  op = VFonctionA f
                          where f (VLitteralA(Entier v)) = VFonctionA g
                                        where g (VLitteralA(Entier w)) = VLitteralA(Entier(op v w))
                                              g e = error ("InterpretationSimple error "++ (show e))
                                f e = error ("InterpretationSimple error "++ (show e)) 

-- Q19

ifthenelseA :: ValeurA
ifthenelseA  = VFonctionA f
                    where f (VLitteralA(Bool p)) = VFonctionA g
                                        where g (VLitteralA x) = VFonctionA h
                                                    where h (VLitteralA y) = VLitteralA(if p then x else y)
                                                          h e = error("InterpretationSimple error "++ (show e))
                                              g e = error("InterpretationSimple error "++ (show e))
                          f e = error("InterpretationSimple error "++ (show e))

-- Q20

main :: IO ()
main = do putStr "InterpreteSimple>"
          commande <- getLine
          if(length commande == 0)
          then exitSuccess
          else
            print(interpreteA envA (ras commande))
          main

-- Q21

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

instance Show ValeurB where
  show (VFonctionB _) = "λ"
  show (VLitteralB (Entier nb)) = show nb
  show (VLitteralB (Bool bool)) = show bool

-- Q22

interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB env (Var x) = case x `lookup` env of
                                     Nothing -> Left ("La variable " ++ x ++ " n'est pas definie")
                                     Just v  -> Right v
interpreteB env (Lit nb) = Right (VLitteralB nb)
interpreteB env (Lam nom expr) = Right (VFonctionB (\v -> interpreteB ((nom,v):env)expr))
interpreteB env (App e1 e2) = case interpreteB env e1 of
                                        e@(Left _)           -> e
                                        Right(VFonctionB f)  -> case interpreteB env e2 of
                                                                          Right v    -> f v
                                                                          e@(Left _) -> e
                                        Right e              -> Left ((show e)++" n'est pas une fonction, application impossible")

-- Q23

addB :: ValeurB
addB = VFonctionB f
       where f (VLitteralB(Entier v)) = Right(VFonctionB g)
                                        where g (VLitteralB(Entier w)) = Right(VLitteralB(Entier(v+w)))
                                              g e = Left((show e)++" n'est pas un entier")
             f e = Left((show e)++" n'est pas un entier")

-- Q24

quotB :: ValeurB
quotB = VFonctionB f
       where f (VLitteralB(Entier v)) = Right(VFonctionB g)
                                        where g (VLitteralB(Entier 0)) = Left("division par zero")
                                              g (VLitteralB(Entier w)) = Right(VLitteralB(Entier(quot v w)))
                                              g e = Left((show e)++" n'est pas un entier")
             f e = Left((show e)++" n'est pas un entier")


-- Q25

data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

instance Show ValeurC where
  show (VFonctionC _) = "λ"
  show (VLitteralC (Entier nb)) = show nb
  show (VLitteralC (Bool bool)) = show bool

-- Q26

interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC env (Lit nb) = ("",VLitteralC nb)
interpreteC env (Lam nom expr) = ("",VFonctionC (\v -> interpreteC ((nom,v):env)expr))
interpreteC env (Var x) = ("",fromJust $ x `lookup` env)
interpreteC env (App expr1 expr2) = case interpreteC env expr1 of
                                     (p,(VFonctionC f)) -> ((p++"."++(fst app)), snd app)
                                                            where app = f (snd(interpreteC env expr2))
                                     e                 -> error("InterpretationTracant error "++show(e))

-- Q27

pingC :: ValeurC
pingC = VFonctionC (\x -> ("p",x))

-- Q28

data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))

data SimpleM v = S v
               deriving Show

interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)
interpreteSimpleM env (Lit nb) = S (VLitteralM nb)
interpreteSimpleM env (Lam nom expr) = S (VFonctionM (\v -> interpreteC ((nom,v):env)expr))
interpreteSimpleM (Var x) = S (fromJust $ x `lookup` env)
interpreteSimpleM (App expr1 expr2) = case interpreteM env expr1 of
                                      