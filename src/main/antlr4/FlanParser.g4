parser grammar FlanParser;

options { tokenVocab=FlanLexer; }

program
  : topDef* EOF
  ;

// Top-level definitions
topDef
  : 'method' IDENT paramList (':' type | 'returns' paramList)?
      requiresSpec* ensuresSpec* block?                                       #methodDef
  | 'const' IDENT '=' expr                                                    #constDef
  | 'type' IDENT '=' type                                                     #typeDef
  | 'lang' IDENT '=' lang                                                     #langDef
  ;

param
  : IDENT ':' type
  ;

paramList
  : '(' (param (',' param)*)? ')'
  ;

requiresSpec
  : 'requires' expr
  ;

ensuresSpec
  : 'ensures' expr
  ;

// Statements
stmt
  : 'var' IDENT (':' type)? ('=' exprOrNondet)? ';'     #varStmt
  | target '=' exprOrNondet ';'                         #assign
  | IDENT augAssignOp expr ';'                          #augAssign
  | IDENT '[' expr ']' ('=' | augAssignOp) expr ';'     #updateAssign
  | expr ';'                                            #exprStmt
  | 'return' expr? ';'                                  #return
  | ifBranch ('else' ifBranch)* ('else' block)?         #if
  | 'while' guard invariantSpec* block                  #while
  | 'for' IDENT 'in' expr invariantSpec* block          #for
  | 'break' ';'                                         #break
  | 'continue' ';'                                      #continue
  | 'abort' expr ';'                                    #abort
  | 'assume' expr ';'                                   #assume
  | 'assert' expr ';'                                   #assert
  ;

exprOrNondet
  : expr
  | '*'
  ;

target
  : IDENT                                     #targetName
  | '(' target (',' target)+ ')'              #tupleTarget
  ;

augAssignOp
  : '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^='
  ;

ifBranch
  : 'if' guard block
  ;

guard
  : expr | '*' | '(' '*' ')'
  ;

block
  : '{' stmt* '}' | stmt
  ;

invariantSpec
  : 'invariant' expr
  ;

// Expressions
expr
  : literal                               #const
  | IDENT                                 #termName
  | '[' exprList ']'                      #seqExpr
  | '{' exprList '}'                      #setExpr
  | '{' itemList '}'                      #mapExpr
  | '(' exprList ')'                      #parenExpr
  | '|' expr '|'                          #size
  | expr '.' IDENT                        #access
  | expr '(' exprList ')'                 #apply
  | expr '[' expr ']'                     #select
  | expr '[' range ']'                    #slice
  | op=(NOT | '-' | '~') expr             #prefixExpr
  | expr op=('*' | '/' | '%') expr        #infixExpr
  | expr op=('+' | '-') expr              #infixExpr
  | expr op=('<<' | '>>') expr            #infixExpr
  | expr op='&' expr                      #infixExpr
  | expr op='^' expr                      #infixExpr
  | expr op='|' expr                      #infixExpr
  | expr op=relOp expr                    #relExpr
  | expr '∈' lang                         #inLang
  | <assoc=right> expr op=AND expr        #infixExpr
  | <assoc=right> expr op=OR expr         #infixExpr
  | <assoc=right> expr op='==>' expr      #infixExpr
  | expr '?' expr ':' expr                #iteExpr
  ;

literal
  : 'null' | 'true' | 'false' | INT_LITERAL | CHAR_LITERAL | STRING_LITERAL
  ;

exprList
  : (expr (',' expr)*)?
  ;

itemList
  : (item (',' item)*)?
  ;

item
  : expr ':' expr
  ;

range
  : start=expr? ':' end=expr?
  ;

relOp
  : '==' | '!=' | '<' | '<=' | '>' | '>=' | 'in' | NOT 'in'
  ;

// Types
type
  : IDENT                             #typeName
  | IDENT '[' type (',' type)* ']'    #genericType
  | '(' (type (',' type)*)? ')'       #parenType
  | <assoc=right> type '|' type       #unionType
  | paramList '->' type               #funType
  | <assoc=right> type '->' type      #funType
  ;

// Languages
lang
  : CHAR_LITERAL                                #singletonLang
  | STRING_LITERAL                              #singletonLang
  | IDENT                                       #langName
  | REGEX_LITERAL                               #regexLang
  | '(' lang ')'                                #parenLang
  | lang '*'                                    #langStar
  | lang '+'                                    #langPlus
  | lang '?'                                    #langOpt
  | lang '{' INT_LITERAL '}'                    #langPower
  | lang '{' INT_LITERAL ',' INT_LITERAL? '}'   #langLoop
  | <assoc=right> lang lang                     #langConcat
  | <assoc=right> lang '|' lang                 #langUnion
  ;
