{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispCore where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Control.Applicative

import LispValues
import LispFunctions

context :: IORef Context
context = unsafePerformIO (newIORef defaultContext)
{-# NOINLINE context #-}


type Context = M.Map String Expr


defaultContext = M.fromList [("+", Fn plus), ("-", Fn minus), ("*", Fn mult), ("/", Fn divide)
                            ,("cons",        Fn cons     )
                            ,("car" ,        Fn car      )
                            ,("cdr" ,        Fn cdr      )
                            ,("nil?",        Fn nil      )
                            ,("eq?" ,        Fn eq       )
                            ,("list",        Fn mkList   )
                            ,("or"  ,        Fn lispOr   )
                            ,("and" ,        Fn lispAnd  )
                            ,("sqrt",        Fn lispSqrt )
                            ,("exp" ,        Fn lispExp  )
                            ,("next-buffer", Fn bufNext  )
                            ]

type REPL = IO

eval :: Expr -> REPL Expr
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
    xs' <- sequence $ map eval xs
    apply x' xs'
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
eval (Eval e)          = eval e
eval e = return e

apply :: Expr -> [Expr] -> REPL Expr
apply (Fn f) xs = return $ f xs
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

apply _ _ = return (Error "invalid expression")

partialEval :: Expr -> REPL Expr
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

getContext :: REPL Context
getContext = liftIO (readIORef context)

putContext :: Context -> REPL ()
putContext ctx  = liftIO (writeIORef context ctx)
