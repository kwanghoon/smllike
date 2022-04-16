module Parser where

import CommonParserUtil
import Token
import Expr

import ParserTime

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction = \rhs -> return ()

-- | 
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Start",
    
    tokenPrecAssoc = [],

    parserSpecList =
    [
      ruleWithNoAction "Start -> Exp",

      ruleWithNoAction "Exp -> AppExp",

      ruleWithNoAction "Exp -> fn identifier => Exp",

      ruleWithNoAction "AppExp -> AtExp",

      ruleWithNoAction "AppExp -> AppExp AtExp",

      ruleWithNoAction "AtExp -> identifier",

      ruleWithNoAction "AtExp -> ( Exp )",

      ruleWithNoAction "AtExp -> let Dec in Exp end",

      ruleWithNoAction "Dec -> val identifier = Exp",
      
      ruleWithNoAction "Dec -> fun identifier identifier = Exp"
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }
  }
