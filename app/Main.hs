module Main where

import CommonParserUtil

import Lexer
import Terminal
import Parser
import Expr

import System.IO

------------------------
-- The syntax completion
------------------------

import Token
import EmacsServer
import SynCompInterface
import Control.Exception

main :: IO ()
main = do
  emacsServer computeCand
  
computeCand :: String -> Bool -> Int -> IO [EmacsDataItem]
computeCand str isSimple cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  successfullyParsed)
  `catch` \e -> case e :: LexError of _ -> handleLexError
  `catch` \e -> case e :: ParseError Token AST of _ -> handleParseError isSimple e)


--------------------
-- The normal parser
--------------------

-- main :: IO ()
-- main = do
--   fileName <- readline "Enter your file: "
--   case fileName of
--     "exit" -> return ()
--     line -> doProcess line

doProcess line = do
  putStrLn ("Opening " ++ line)
  text <- readFile line
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec text
  putStrLn "Parsing..."
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Pretty Printing..."
  putStrLn (show exprSeqAst)
  
  
readline msg = do
  putStr msg
  hFlush stdout
  readline'

readline' = do
  ch <- getChar
  if ch == '\n' then
    return ""
  else
    do line <- readline'
       return (ch:line)


