parser grammar FlanParser;

options { tokenVocab=FlanLexer; }

program
  : topDef* EOF
  ;

// Top-level definitions
topDef
  : 'method' IDENT paramList (':' type)? requiresSpec* ensuresSpec* block?    #methodDef
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
  | IDENT '=' exprOrNondet ';'                          #assign
  | IDENT augAssignOp expr ';'                          #augAssign
  | expr ';'                                            #exprStmt
  | 'return' expr? ';'                                  #return
  | ifBranch ('else' ifBranch)* ('else' block)?         #if
  | 'while' guard invariantSpec* block                  #while
  | 'break' ';'                                         #break
  | 'continue' ';'                                      #continue
  | 'assume' expr ';'                                   #assume
  | 'assert' expr ';'                                   #assert
  ;

exprOrNondet
  : expr
  | '*'
  ;

augAssignOp
  : '+=' | '-=' | '*=' | '&=' | '|=' | '^='
  ;

ifBranch
  : 'if' guard block
  ;

guard
  : expr | '*' | '(' '*' ')'
  ;

block
  : '{' stmt* '}'
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
  | expr '[' expr '=' expr ']'            #update
  | op=('!' | '-' | '~') expr             #prefixExpr
  | expr op='*' expr                      #infixExpr
  | expr op=('+' | '-') expr              #infixExpr
  | expr op=('<<' | '>>') expr            #infixExpr
  | expr op='&' expr                      #infixExpr
  | expr op='^' expr                      #infixExpr
  | expr op='|' expr                      #infixExpr
  | expr op=relOp expr                    #relExpr
  | <assoc=right> expr op='&&' expr       #infixExpr
  | <assoc=right> expr op='||' expr       #infixExpr
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
  : '==' | '!=' | '<' | '<=' | '>' | '>=' | 'in' | '!' 'in'
  ;

// Types
type
  : NULL                              #nullType
  | BOOL                              #boolType
  | INT                               #intType
  | CHAR                              #charType
  | STRING                            #stringType
  | SEQ '[' type ']'                  #seqType
  | SET '[' type ']'                  #setType
  | MAP '[' type ',' type ']'         #mapType
  | IDENT                             #typeName
  | '(' (type (',' type)*)? ')'       #parenType
  | type '?'                          #optType
  | <assoc=right> type '|' type       #unionType
  | paramList '->' type               #funType
  | <assoc=right> type '->' type      #funType
  ;

// Languages
lang
  : CHAR_LITERAL                    #singletonLang
  | STRING_LITERAL                  #singletonLang
  | IDENT                           #langName
  | REGEX_LITERAL                   #regexLang
  | '(' lang ')'                    #parenLang
  | <assoc=right> lang lang         #langConcat
  | <assoc=right> lang '|' lang     #langUnion
  ;
