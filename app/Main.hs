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
import SynCompInterface
import EmacsServer

import Data.Typeable
import Control.Exception
import Data.List (nub)

main :: IO ()
main = do
  emacsServer computeCand
  
-- Computing candidates for syntax completion

computeCand :: String -> Bool -> Int -> IO [EmacsDataItem]
computeCand str isSimple cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  return [SuccessfullyParsed])
  `catch` \e -> case e :: LexError of _ -> return [SynCompInterface.LexError])
  `catch` \e -> case e :: ParseError Token AST of
                  NotFoundAction _ state stk actTbl gotoTbl prodRules pFunList terminalList ->
                    if length terminalList  == 1 then do -- [$]
                      candidates <- compCandidates isSimple 0 [] state actTbl gotoTbl prodRules pFunList stk -- return ["candidates"]
                      let cands = candidates
                      let strs = nub [ concatStrList strList | strList <- map (map showSymbol) cands ]
                      let rawStrs = nub [ strList | strList <- map (map showRawSymbol) cands ]
                      mapM_ (putStrLn . show) rawStrs
                      return $ map Candidate strs
                    else
                      return [SynCompInterface.ParseError (map terminalToString terminalList)]
                  NotFoundGoto state _ stk actTbl gotoTbl prodRules pFunList terminalList ->
                    if length terminalList == 1 then do -- [$]
                      candidates <- compCandidates isSimple 0 [] state actTbl gotoTbl prodRules pFunList stk
                      let cands = candidates
                      let strs = nub [ concatStrList strList | strList <- map (map showSymbol) cands ]
                      let rawStrs = nub [ strList | strList <- map (map showRawSymbol) cands ]
                      mapM_ (putStrLn . show) rawStrs
                      return $ map Candidate strs
                    else
                      return [SynCompInterface.ParseError (map terminalToString terminalList)]
                      
showSymbol (TerminalSymbol s) = s
showSymbol (NonterminalSymbol _) = "..."

showRawSymbol (TerminalSymbol s) = s
showRawSymbol (NonterminalSymbol s) = s

concatStrList [] = "" -- error "The empty candidate?"
concatStrList [str] = str
concatStrList (str:strs) = str ++ " " ++ concatStrList strs


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


