module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction = \rhs -> ()

-- | 
parserSpec :: ParserSpec Token AST
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
    genparserexe = "yapb-exe"
  }
