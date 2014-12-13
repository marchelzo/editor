{-# LANGUAGE ForeignFunctionInterface #-}

module LispFunctions where

import Foreign.C.String
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce
import Control.DeepSeq (force)
import Data.Char (toLower, toUpper)
import Control.Monad
import qualified Text.Regex.Posix as Regex
import Data.Char (chr,ord)

import LispValues

plus :: [Expr] -> IO Expr
plus = return . Number . sum . map (\(Number x) -> x)

minus :: [Expr] -> IO Expr
minus [Number x, Number y] = return $ Number (x - y)

mult :: [Expr] -> IO Expr
mult = return . Number . product . map (\(Number x) -> x)

divide :: [Expr] -> IO Expr
divide [Number x, Number y] = return $ Number (x / y)

modulo :: [Expr] -> IO Expr
modulo [Number x, Number y] = return $ Number (fromIntegral (round x `mod` round y))

lispSqrt :: [Expr] -> IO Expr
lispSqrt [Number x] = return $ Number (sqrt x)

lispExp :: [Expr] -> IO Expr
lispExp [Number x] = return $ Number (exp x)

cons :: [Expr] -> IO Expr
cons [x, Quoted (List xs)] = return $ Quoted (List (x:xs))

car :: [Expr] -> IO Expr
car [Quoted (List (x:_))] = return x

cdr :: [Expr] -> IO Expr
cdr [Quoted (List (_:xs))] = return $ Quoted (List xs)
cdr e                      = return $ Error ("error: cdr takes a pair, given: " ++ show e)

nil :: [Expr] -> IO Expr
nil [(Quoted (List []))] = return $ Bool True
nil _                    = return $ Bool False

lispIf :: [Expr] -> IO Expr
lispIf [(Bool False), _, x] = return x
lispIf [_, x, _]            = return x

eq :: [Expr] -> IO Expr
eq [Number x, Number y] = return $ Bool (x == y)
eq [String x, String y] = return $ Bool (x == y)
eq [_, _]               = return $ Bool False
eq _                    = return $ Error "error: invalid arguments to eq?"

lt [Number x, Number y] = return $ Bool (x < y)

gt [Number x, Number y] = return $ Bool (x > y)

range [Number j, Number k, Number i] = let nums = map (Number . fromIntegral)  [(round j),(round k)..(round i)]
                                           in return (Quoted (List nums))
range [Number j, Number k] = let nums = map (Number . fromIntegral)  [(round j)..(round k)]
                                           in return (Quoted (List nums))
range [Number j] = let nums = map (Number . fromIntegral)  [0..(round j)]
                                           in return (Quoted (List nums))

mkList :: [Expr] -> IO Expr
mkList xs = return $ Quoted (List xs)

lispAnd :: [Expr] -> IO Expr
lispAnd [Bool p, Bool q] = return $ Bool (p && q)
lispAnd _                = return $ Error "error: and requires two boolean arguments"

lispOr :: [Expr] -> IO Expr
lispOr [Bool p, Bool q] = return $ Bool (p || q)
lispOr _                = return $ Error "error: or requires two boolean arguments"

lOrd [String s] = return $ Number (fromIntegral (ord (head s)))
lChr [Number k] = return $ String (return (chr (round k)))

strTake [Number x, String s] = return $ String (take (round x) s)

strDrop [Number x, String s] = return $ String (drop (round x) s)

substr [Number i, Number j, String s] = return $ String ((take (round (j - i)) . drop (round i)) s)

strCat [String s1, String s2] = return $ String (s1 ++ s2)

strToUp [String s] = return $ String (map toUpper s)

strToLow [String s] = return $ String (map toLower s)

strLen [String s] = return $ (Number . fromIntegral . length) s

matches [String s, String r] = return $ Bool (s Regex.=~ r :: Bool)

matchesList [String s, String r] = (return . listToList . map (listToList . map String)) (s Regex.=~ r :: [[String]])

listToList :: [Expr] -> Expr
listToList xs = Quoted (List xs)


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
foreign import ccall "../lispbindings.h get_char" lgetChar' :: IO CChar
foreign import ccall "../lispbindings.h line_number" lineNumber' :: IO CSize
foreign import ccall "../lispbindings.h current_line" currentLine' :: IO CString
foreign import ccall "../lispbindings.h get_nth_line" getNthLine' :: CSize -> IO CString
foreign import ccall "../lispbindings.h column_number" columnNumber' :: IO CSize
foreign import ccall "../lispbindings.h indent_line" indentLine' :: CInt -> IO ()
foreign import ccall "../lispbindings.h go_to_col" goToCol' :: CSize -> IO ()

goToCol [Number x] = goToCol' (round x) >> return Bottom

indentLine [Number n] = indentLine' (round n) >> return Bottom

columnNumber _ = (Number . fromIntegral) `liftM` columnNumber'

lineNumber _ = (Number . fromIntegral) `liftM` lineNumber'

currentLine _ = String `liftM` (currentLine' >>= peekCString)

getNthLine [Number x] = do cs <- getNthLine' (round x)
                           s  <- peekCString cs
                           return (String s)

lgetChar :: [Expr] -> IO Expr
lgetChar _ = fmap ((String . return) . (unsafeCoerce :: CChar -> Char)) lgetChar'

bufNext :: [Expr] -> IO Expr
bufNext [] = nextBuffer >> return Bottom

bufPrev :: [Expr] -> IO Expr
bufPrev [] = prevBuffer >> return Bottom

bufNew [String s] = do cs <- newCString s
                       bufNew' cs
                       return Bottom

normalEval [String s] = do cs <- newCString s
                           normalEval' cs
                           return Bottom

evalBuffer [] = evalBuffer' >> return Bottom

normalMap  [String fr, String to] = do [cFr, cTo] <- mapM newCString [fr,to]
                                       nmap cFr cTo
                                       return Bottom
visualMap  [String fr, String to] = do [cFr, cTo] <- mapM newCString [fr,to]
                                       vmap cFr cTo
                                       return Bottom
commandMap  [String fr, String to] = do [cFr, cTo] <- mapM newCString [fr,to]
                                        cmap cFr cTo
                                        return Bottom
insertMap  [String fr, String to] = do [cFr, cTo] <- mapM newCString [fr,to]
                                       imap cFr cTo
                                       return Bottom
