{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Chapter1.Interpreter 
  ( Id 
  , Binop(..)
  , Stm(..)
  , Exp(..)
  , interp
  ) where

import Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString(..))
import Control.Monad.State
import Data.List
import Data.Maybe

type Id = String

data Binop = Plus | Minus | Times | Div
  deriving (Eq, Show)

infixl 2 :=

data Stm 
  = CompoundStm Stm Stm
  | (:=) Id Exp
  | PrintStm [Exp] 
  deriving (Eq, Show)

data Exp  
  = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp
  deriving (Eq, Show)

instance Num Exp where
  fromInteger = NumExp . fromInteger
  a + b = OpExp a Plus b  
  a - b = OpExp a Minus b  
  a * b = OpExp a Times b
  abs = undefined  
  signum = undefined
 
instance Fractional Exp where
  a / b = OpExp a Div b
  fromRational = undefined

instance IsString Exp where
  fromString = IdExp  

instance Semigroup Stm where
  (<>) = CompoundStm

instance Monoid Stm where
  mappend = (<>)
  mempty = undefined

maxArgs :: Stm -> Int
maxArgs (CompoundStm a b) = max (maxArgs a) (maxArgs b)
maxArgs (_ := e) = maxArgsE e
maxArgs (PrintStm es) = length es

maxArgsE :: Exp -> Int
maxArgsE (IdExp _) = 0
maxArgsE (NumExp _) = 0
maxArgsE (OpExp l _ r) = maxArgsE l `max` maxArgsE r
maxArgsE (EseqExp s e) = maxArgs s `max` maxArgsE e

interp :: Stm -> IO ()
interp s = putStrLn =<< fmap showState (execStateT (interpS s) Map.empty)

interpS :: Stm -> StateT (Map Id Int) IO ()
interpS (CompoundStm s1 s2) = interpS s1 >> interpS s2
interpS (v := e) = do
  e' <- interpE e
  modify (Map.insert v e')
interpS (PrintStm es) = do
  vs <- traverse interpE es
  liftIO . putStrLn . unwords $ map show vs

interpE :: Exp -> StateT (Map Id Int) IO Int
interpE (IdExp v) = do
  m <- get
  pure . fromMaybe (error $ "Variable " ++ v ++ " was not assigned a value") 
       $ Map.lookup v m
interpE (NumExp i) = return i
interpE (OpExp l Plus r)  = (+) <$> interpE l <*> interpE r
interpE (OpExp l Minus r) = (-) <$> interpE l <*> interpE r
interpE (OpExp l Times r) = (*) <$> interpE l <*> interpE r
interpE (OpExp l Div r)   = div <$> interpE l <*> interpE r
interpE (EseqExp s e) = interpS s >> interpE e

showState :: Map Id Int -> String
showState m = intercalate ", " $ map (\(k,v) -> k ++ " = " ++ show v) $ Map.assocs m

-----------------
-- Demo programs
-----------------

prog :: Stm
prog = 
  ("a" := OpExp (NumExp  5) Plus (NumExp 3)) `CompoundStm` 
  ("b" := EseqExp 
      (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
      (OpExp (NumExp 10) Times (IdExp "a"))
  ) `CompoundStm`
  PrintStm [IdExp "b"]

prog1 :: Stm
prog1 = do
  "a" := 5 + 3 
  "b" := EseqExp
          (PrintStm ["a", "a" - 1])
          (10 * "a")
  PrintStm ["b"]        
  where (>>) = CompoundStm        

printTest :: Stm   
printTest = 
  PrintStm 
    [ EseqExp (PrintStm [1 * 10]) 1 
    , EseqExp (PrintStm [2 * 10]) 2 
    , EseqExp (PrintStm [3 * 10]) 3 
    ]
