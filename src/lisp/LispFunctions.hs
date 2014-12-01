{-# LANGUAGE ForeignFunctionInterface #-}

module LispFunctions where

import Foreign.C.String
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq (force)
import Data.Char (toLower, toUpper)

import LispValues

plus :: [Expr] -> Expr
plus = Number . sum . map (\(Number x) -> x)

minus :: [Expr] -> Expr
minus [Number x, Number y] = Number (x - y)

mult :: [Expr] -> Expr
mult = Number . product . map (\(Number x) -> x)

divide :: [Expr] -> Expr
divide [Number x, Number y] = Number (x / y)

lispSqrt :: [Expr] -> Expr
lispSqrt [Number x] = Number (sqrt x)

lispExp :: [Expr] -> Expr
lispExp [Number x] = Number (exp x)

cons :: [Expr] -> Expr
cons [x, Quoted (List xs)] = Quoted (List (x:xs))

car :: [Expr] -> Expr
car [Quoted (List (x:_))] = x

cdr :: [Expr] -> Expr
cdr [Quoted (List (_:xs))] = Quoted (List xs)
cdr e                      = Error ("error: cdr takes a pair, given: " ++ show e)

nil :: [Expr] -> Expr
nil [(Quoted (List []))] = Bool True
nil _                    = Bool False

lispIf :: [Expr] -> Expr
lispIf [(Bool False), _, x] = x
lispIf [_, x, _]            = x

eq :: [Expr] -> Expr
eq [Number x, Number y] = Bool (x == y)
eq [String x, String y] = Bool (x == y)
eq [_, _]               = Bool False
eq _                    = Error "error: eq takes two arguments that"

mkList :: [Expr] -> Expr
mkList xs = Quoted (List xs)

lispAnd :: [Expr] -> Expr
lispAnd [Bool p, Bool q] = Bool (p && q)
lispAnd _                = Error "error: and requires two boolean arguments"

lispOr :: [Expr] -> Expr
lispOr [Bool p, Bool q] = Bool (p || q)
lispOr _                = Error "error: or requires two boolean arguments"

strTake [Number x, String s] = String (take (round x) s)

strDrop [Number x, String s] = String (drop (round x) s)

substr [Number i, Number j, String s] = String ((take (round (j - i)) . drop (round i)) s)

strCat [String s1, String s2] = String (s1 ++ s2)

strToUp [String s] = String (map toUpper s)

strToLow [String s] = String (map toLower s)

strLen [String s] = (Number . fromIntegral . length) s


-- | Foreign C functions to mutate the editor state  |
---------------------------------------------------- |

foreign import ccall "../lispbindings.h next_buffer" nextBuffer :: IO ()
foreign import ccall "../lispbindings.h prev_buffer" prevBuffer :: IO ()
foreign import ccall "../lispbindings.h new_buffer" bufNew' :: CString -> IO ()
foreign import ccall "../lispbindings.h normal_eval" normalEval' :: CString -> IO ()
foreign import ccall "../lispbindings.h eval_buffer" evalBuffer' :: IO ()
foreign import ccall "../lispbindings.h normal_map" nmap :: CString -> CString -> IO ()
foreign import ccall "../lispbindings.h insert_map" imap :: CString -> CString -> IO ()
foreign import ccall "../lispbindings.h visual_map" vmap :: CString -> CString -> IO ()
foreign import ccall "../lispbindings.h command_map" cmap :: CString -> CString -> IO ()

bufNext :: [Expr] -> Expr
bufNext [] = seq (unsafePerformIO nextBuffer) (Number 1)

bufPrev :: [Expr] -> Expr
bufPrev [] = seq (unsafePerformIO prevBuffer) (Number 1)

bufNew [String s] = force $ seq (force (unsafePerformIO (bufNew' (unsafePerformIO (newCString s))))) (Number 1)

normalEval [String s] = seq (force (unsafePerformIO (normalEval' (unsafePerformIO (newCString s))))) (Number 1)

evalBuffer [] = seq (unsafePerformIO evalBuffer') (Number 1)

keyMap [(Quoted (Symbol m)), (String from), (String to)] = case m of
    "normal-map" -> seq (force (unsafePerformIO (nmap (unsafePerformIO (newCString from))
                                                      (unsafePerformIO (newCString to))))) (Number 1)
    "command-map" -> seq (force (unsafePerformIO (cmap (unsafePerformIO (newCString from))
                                                      (unsafePerformIO (newCString to))))) (Number 1)
    "insert-map" -> seq (force (unsafePerformIO (imap (unsafePerformIO (newCString from))
                                                      (unsafePerformIO (newCString to))))) (Number 1)
    "visual-map" -> seq (force (unsafePerformIO (vmap (unsafePerformIO (newCString from))
                                                      (unsafePerformIO (newCString to))))) (Number 1)

