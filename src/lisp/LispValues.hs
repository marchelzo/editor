module LispValues where

import Data.List (intersperse)
import Text.Printf (printf)
import Control.DeepSeq

data Expr = Number Double
          | String String
          | Symbol String
          | Define String Expr
          | Eval Expr
          | Quoted Expr
          | List [Expr]
          | Bool Bool
          | If Expr Expr Expr
          | Begin [Expr]
          | Procedure Expr
          | Bottom
          | Lambda [String] Expr
          | Fn ([Expr] -> IO Expr)
          | Error String

instance Show Expr where
    show (Fn _)       = "<procedure>"
    show (Lambda _ _) = "<procedure>"
    show (Number x)   = showNum x
    show (String s)   = show s
    show (List xs)    = show xs
    show (Symbol s)   = s
    show Bottom       = ""
    show (Define _ _) = "<define>"
    show (Bool True)  = "#t"
    show (Bool False) = "#f"
    show (Error e)    = e
    show (Quoted q)   = showQuoted q
    show _            = "<procedure>"

showNum :: Double -> String
showNum x
    | x == fromIntegral (round x) && ((not . elem 'e') (show x)) = takeWhile (/='.') (show x)
    | otherwise                   = printf "%-10f" x

showQuoted :: Expr -> String
showQuoted (List xs) = "(" ++ concat (intersperse " " $ (map show xs)) ++ ")"
showQuoted q         = show q
