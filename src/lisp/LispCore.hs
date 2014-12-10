{-# LANGUAGE FlexibleInstances #-}

module LispCore where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.DeepSeq

import LispValues
import LispFunctions

context :: IORef Context
context = unsafePerformIO (newIORef defaultContext)
{-# NOINLINE context #-}


type Context = M.Map String Expr


defaultContext = M.fromList [("+", Fn plus), ("-", Fn minus), ("*", Fn mult), ("/", Fn divide)
                            ,("cons",        Fn cons         )
                            ,("car" ,        Fn car          )
                            ,("cdr" ,        Fn cdr          )
                            ,("nil?",        Fn nil          )
                            ,("eq?" ,        Fn eq           )
                            ,("gt?",         Fn gt           )
                            ,("lt?",         Fn lt           )
                            ,("list",        Fn mkList       )
                            ,("range",       Fn range        )
                            ,("or"  ,        Fn lispOr       )
                            ,("and" ,        Fn lispAnd      )
                            ,("sqrt",        Fn lispSqrt     )
                            ,("exp" ,        Fn lispExp      )
                            ,("strlen",      Fn strLen       )
                            ,("strtake",     Fn strTake      )
                            ,("substr",      Fn substr       )
                            ,("strdrop",     Fn strDrop      )
                            ,("strcat",      Fn strCat       )
                            ,("toupper",     Fn strToUp      )
                            ,("tolower",     Fn strToLow     )
                            ,("next-buffer", Fn bufNext      )
                            ,("prev-buffer", Fn bufPrev      )
                            ,("new-buffer",  Fn bufNew       )
                            ,("normal",      Fn normalEval   )
                            ,("eval-buffer", Fn evalBuffer   )
                            ,("key-map",     Fn keyMap       )
                            ,("get-char",    Fn lgetChar     )
                            ]

eval :: Expr -> IO Expr
eval (Number x) = return (Number x)
eval (String s) = return (String s)
eval (Symbol s) = do
    ctx <- getContext
    let res = M.lookup s ctx
    return $ case res of
        Just e -> e
        _      -> Error ("reference to undefined symbol: `" ++ s ++ "`")
eval (List (x:xs)) = do
    x'  <- eval x
    xs' <- mapM eval xs
    force (apply x' xs')
eval (Fn f) = return (Fn f)
eval (Lambda cs e) = return (Lambda cs e)
eval (Define s e) = do
    ctx <- getContext
    expr <- eval e
    let newCtx = M.insert s expr ctx
    putContext newCtx
    return Bottom
eval (Bool b) = return (Bool b)
eval (If cond a b) = do
    bool <- eval cond
    case bool of
        (Bool False) -> eval b
        _            -> eval a
eval (Quoted (Number x)) = return (Number x)
eval (Quoted e) = return (Quoted e)
eval (Eval (Quoted e)) = eval e
eval (Eval e)          = eval e >>= eval
eval (Begin es)        = evalProgram es
eval (Procedure e)     = seq (force (eval e)) (return (Procedure e))
eval e = return e

evalProgram :: [Expr] -> IO Expr
evalProgram [] = return (String "")
evalProgram [e] = eval e
evalProgram (e:es) = do e' <- (force (eval e))
                        e' `seq` evalProgram es

apply :: Expr -> [Expr] -> IO Expr
apply (Fn f) xs = f xs
apply (Lambda cs (Lambda ics e)) xs = do
    ctx <- getContext
    let lambdaContext = M.fromList $ zip cs xs
    let newCtx = M.union lambdaContext ctx
    putContext newCtx
    result <- partialEval e
    putContext ctx
    return $ Lambda ics result

apply (Lambda cs e) xs = do
    ctx <- getContext
    let lambdaContext = M.fromList $ zip cs xs
    let newCtx = M.union lambdaContext ctx
    putContext newCtx
    result <- eval e
    putContext ctx
    return result

apply (Procedure p) _ = eval p

apply _ _ = return (Error "invalid expression")

partialEval :: Expr -> IO Expr
partialEval (Symbol s) = do
    ctx <- getContext
    let res = M.lookup s ctx
    return $ case res of
        Just r -> r
        _      -> Symbol s
partialEval (List xs) = do
    xs' <- sequence $ map partialEval xs
    return $ List xs'
partialEval x = return x

getContext :: IO Context
getContext = readIORef context

putContext :: Context -> IO ()
putContext ctx  = writeIORef context ctx

---------------------------------
-- | NFData instance for Expr

instance NFData (IO Expr) where
    rnf e = seq (fmap eval e) ()

instance NFData Expr where
    rnf e = seq (unsafePerformIO (eval e)) ()
