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

computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode =
  (
   (
    (do ast <- parsing False
                 parserSpec ((), 1, 1, programTextUptoCursor)
                   (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
        successfullyParsed)

    `catch` \e -> case e :: LexError of _ -> handleLexError)
    
    `catch` \e ->
      case e :: ParseError Token AST () of
        _ ->
          do let (_,line,column,programTextAfterCursor) = lpStateFrom e
             handleParseError (
               defaultHandleParseError {
                   debugFlag=debug,
                   searchMaxLevel=maxLevel,
                   simpleOrNested=isSimpleMode,
                   postTerminalList=[],   -- terminalListAfterCursor is set to []!
                   nonterminalToStringMaybe=Nothing }) e
             )
