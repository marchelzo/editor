{-# LANGUAGE ForeignFunctionInterface #-}

module EditorLisp where

import LispCore
import Parser
import LispValues
import Data.List (intersperse)

import Foreign.C.String
import Foreign.C

lispEval :: CString -> IO CString
lispEval s = do expr <- fmap readProgram (peekCString s)
                case expr of
                    Left er  -> (newCString . concat . intersperse " " . lines) ("syntax error:\n" ++ show er)
                    Right es -> do result <- evalProgram es
                                   newCString (show result)

foreign export ccall lispEval :: CString -> IO CString
