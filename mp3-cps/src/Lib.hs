--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\v -> k (n * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ke ko | even x    = ke x
                   | otherwise = ko x
evenoddk (x:xs) ke ko | even x    = evenoddk xs (\v -> ke (v + x)) ko
                      | otherwise = evenoddk xs ke (\v -> ko (v + x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple e = case e of
    AppExp _ _ -> False
    IntExp _   -> True
    VarExp _   -> True
    IfExp e1 e2 e3 -> isSimple e1 && isSimple e2 && isSimple e3
    OpExp _ e1 e2 -> isSimple e1 && isSimple e2
    _ -> True


--- ### Define `cpsExp` - Overview


cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k cnt = (AppExp k (IntExp i), cnt)
cpsExp (VarExp v) k cnt = (AppExp k (VarExp v), cnt)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k cnt
    | isSimple e = (AppExp (AppExp f e) k, cnt)
    | otherwise =
        let (x, cnt2) = gensym cnt in
        cpsExp e (LamExp x (AppExp (AppExp f (VarExp x)) k)) cnt2

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k cnt
    | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), cnt)
    | isSimple e2 = 
        let (x, cnt2) = gensym cnt in
        cpsExp e1 (LamExp x (AppExp k (OpExp op (VarExp x) e2))) cnt2
    | isSimple e1 = 
        let (x, cnt2) = gensym cnt in
        cpsExp e2 (LamExp x (AppExp k (OpExp op e1 (VarExp x)))) cnt2
    | otherwise = 
        let (x1, cnt15)    = gensym cnt in
        let (x2, cnt2)     = gensym cnt15 in
        let (cpsexp, cnt3) = cpsExp e2 (LamExp x2 (AppExp k (OpExp op (VarExp x1) (VarExp x2)))) cnt2 in
        cpsExp e1 (LamExp x1 cpsexp) cnt3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k cnt
    | isSimple e1 = (IfExp e1 (fst (cpsExp e2 k cnt)) (fst (cpsExp e3 k cnt)), cnt)
    | otherwise = 
        let (x, cnt2) = gensym cnt in
        let (cpsexp1, cnt3) = cpsExp e2 k cnt2 in
        let (cpsexp2, cnt4) = cpsExp e3 k cnt3 in
        cpsExp e1 (LamExp x (IfExp (VarExp x) cpsexp1 cpsexp2)) cnt4

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl s ss e) = Decl s (ss++["k"]) (fst $ cpsExp e (VarExp "k") 0)