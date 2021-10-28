module Main where

import CommonParserUtil

import TokenInterface
import Token
import Expr
import Lexer
import Parser

import EmacsServer
import SynCompInterface
import Control.Exception
import Data.Typeable

import System.IO

main :: IO ()
main = do
  emacsServer (computeCand False)

maxLevel = 10000

data ParseErrorWithLineCol token ast = ParseErrorWithLineCol Int Int (ParseError token ast)
  deriving (Typeable, Show)

instance (TokenInterface token, Typeable token, Show token, Typeable ast, Show ast)
  => Exception (ParseErrorWithLineCol token ast)

computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode = do
  ((computeCand_ isSimpleMode programTextUptoCursor programTextAfterCursor
    `catch` \e -> case e :: LexError of _ -> handleLexError)
    `catch` \e -> case e :: ParseErrorWithLineCol Token AST of ParseErrorWithLineCol line column e -> do {
        (_, _, terminalListAfterCursor) <- lexingWithLineColumn lexerSpec line column programTextAfterCursor;
        handleParseError (
          HandleParseError {
              debugFlag=debug,
              searchMaxLevel=maxLevel,
              simpleOrNested=isSimpleMode,
              postTerminalList=terminalListAfterCursor,
              nonterminalToStringMaybe=Nothing}) e
        })
    
computeCand_ :: Bool -> String -> String -> IO [EmacsDataItem]
computeCand_ isSimpleMode programTextUptoCursor programTextAfterCursor = do
  (line, column, terminalListUptoCursor)  <- lexingWithLineColumn lexerSpec 1 1 programTextUptoCursor
  
  ast <-
    (parsing False parserSpec terminalListUptoCursor
      `catch` \e -> case e :: ParseError Token AST of  _ -> throw (ParseErrorWithLineCol line column e))

  successfullyParsed

-- computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
-- computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode = ((do
--   (line, column, terminalList) <- lexingWithLineColumn lexerSpec 1 1 programTextUptoCursor 
--   ast <- parsing False parserSpec terminalList 
--   successfullyParsed)
--   `catch` \e -> case e :: LexError of _ -> handleLexError)
--   `catch` \e ->
--      case e :: ParseErrorWithLineCol Token AST of
--        ParseErrorWithLineCol line column e ->
--           do (_,_,terminalListAfterCursor) <-
--                 lexingWithLineColumn lexerSpec line column programTextAfterCursor
            
--              handleParseError (
--                HandleParseError {
--                    debugFlag=debug,
--                    searchMaxLevel=maxLevel,
--                    simpleOrNested=isSimpleMode,
--                    postTerminalList=terminalListAfterCursor,
--                    nonterminalToStringMaybe=Nothing}) e
  

