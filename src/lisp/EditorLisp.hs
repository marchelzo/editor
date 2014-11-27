{-# LANGUAGE ForeignFunctionInterface #-}

module EditorLisp where

import LispCore
import Parser

import Foreign.C.String
import Foreign.C

lispEval :: CString -> IO CString
lispEval s = do expr <- fmap readExpr (peekCString s)
                case expr of
                    Left er -> newCString ("syntax error:\n" ++ show er)
                    Right e -> do result <- eval e
                                  newCString (show result)

foreign export ccall lispEval :: CString -> IO CString
