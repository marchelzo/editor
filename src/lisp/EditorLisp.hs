{-# LANGUAGE ForeignFunctionInterface #-}

module EditorLisp where

import LispCore
import Parser
import LispValues

import Foreign.C.String
import Foreign.C

evalProgram :: [Expr] -> IO String
evalProgram [] = return ""
evalProgram [e] = fmap show (eval e)
evalProgram (e:es) = eval e >> evalProgram es

lispEval :: CString -> IO CString
lispEval s = do expr <- fmap readProgram (peekCString s)
                case expr of
                    Left er  -> newCString ("syntax error:\n" ++ show er)
                    Right es -> do result <- evalProgram es
                                   newCString result

foreign export ccall lispEval :: CString -> IO CString
