parser grammar FlanParser;

options { tokenVocab=FlanLexer; }

program
  : topDef* EOF
  ;

// Top-level definitions
topDef
  : 'type' IDENT '=' type                                   #typeDef
  | 'lang' IDENT '=' clause                                 #langDef
  | 'const' IDENT '=' expr                                  #constDef
  | 'method' IDENT paramList (':' type)? methodSpec* stmt   #methodDef
  ;

param
  : IDENT ':' type
  ;

paramList
  : OPEN_PAREN (param (COMMA param)*)? CLOSE_PAREN
  ;

methodSpec
  : 'requires' expr   #requireSpec
  | 'ensures' expr    #ensureSpec
  ;

// Types
type
  : IDENT                                     #typeName
  | OPEN_PAREN typeList? CLOSE_PAREN          #parenType
  | type OPEN_SQUARE typeList CLOSE_SQUARE    #typeApply
  | type QUESTION                             #optType
  | <assoc=right> type VERT type              #unionType
  | <assoc=right> type '->' type              #funType
  ;

typeList
  : type (COMMA type)*
  ;

// Languages
clause
  : CHAR                                  #charClause
  | STRING                                #stringClause
  | IDENT                                 #langName
  | charClass                             #charClassClause
  | RE_BEGIN regEx RE_END                 #regExClause
  | OPEN_PAREN clause CLOSE_PAREN         #parenClause
  | clause quant                          #repeat
  | <assoc=right> clause clause           #concat
  | <assoc=right> clause VERT clause      #union
  ;

quant
  : STAR                                    #star
  | PLUS                                    #plus
  | QUESTION                                #opt
  | OPEN_CURLY INT CLOSE_CURLY              #exactly
  | OPEN_CURLY INT COMMA INT? CLOSE_CURLY   #between
  ;

regEx
  : RE_CHAR                             #RE_Char
  | RE_CHAR_TYPE                        #RE_CharType
  | charClass                           #RE_CharClass
  | OPEN_PAREN regEx CLOSE_PAREN        #RE_Paren
  | regEx quant                         #RE_Repeat
  | <assoc=right> regEx regEx           #RE_Concat
  | <assoc=right> regEx VERT regEx      #RE_Union
  ;

charClass
  : CC_BEGIN charClassAtom* CC_END
  ;

charClassAtom
  : CC_CHAR                   #CC_Char
  | CC_CHAR CC_DASH CC_CHAR   #CC_Range
  | CC_CHAR_TYPE              #CC_CharType
  ;

// Expressions
expr
  : literal                                           #const
  | IDENT                                             #termName
  | OPEN_PAREN exprList CLOSE_PAREN                   #parenExpr
  | OPEN_PAREN expr ':' type CLOSE_PAREN              #annotExpr
  | expr DOT IDENT                                    #memberAccess
  | expr OPEN_PAREN exprList CLOSE_PAREN              #apply
  | expr OPEN_SQUARE expr CLOSE_SQUARE                #at
  | op=(NOT | MINUS | BIT_NOT) expr                   #prefixExpr
  | expr op=STAR expr                                 #infixExpr
  | expr op=(PLUS | MINUS) expr                       #infixExpr
  | expr op=('<<' | '>>') expr                        #infixExpr
  | expr op=BIT_AND expr                              #infixExpr
  | expr op=BIT_XOR expr                              #infixExpr
  | expr op=VERT expr                                 #infixExpr
  | expr (relOp expr)+                                #relExpr
  | <assoc=right> expr op='&&' expr                   #infixExpr
  | <assoc=right> expr op='||' expr                   #infixExpr
  | <assoc=right> expr op='==>' expr                  #infixExpr
  | expr '->' expr                                    #arrowExpr
  | expr QUESTION expr COLON expr                     #condExpr
  | 'lambda' paramList COMMA expr                     #lambda
  ;

exprList
  : /* empty */
  | expr (COMMA expr)*
  ;

relOp
  : '==' | '!=' | '<' | '<=' | '>' | '>=' | 'in' | '!' 'in'
  ;

// Literals
literal
  : 'null'    #null
  | 'true'    #true
  | 'false'   #false
  | INT       #int
  | CHAR      #char
  | STRING    #string
  ;

// Statements
stmt
  : 'var' IDENT (':' type)? ('=' value)? ';'                      #declare
  | IDENT '=' value ';'                                           #assign
  | IDENT augOp value ';'                                         #augAssign
  | expr ';'                                                      #exprStmt
  | 'assume' expr ';'                                             #assume
  | 'assert' expr ';'                                             #assert
  | 'return' expr? ';'                                            #return
  | 'if' OPEN_PAREN expr CLOSE_PAREN stmt ('else' stmt)?          #if
  | 'while' OPEN_PAREN expr CLOSE_PAREN loopSpec* stmt            #while
  | 'for' OPEN_PAREN IDENT 'in' expr CLOSE_PAREN loopSpec* stmt   #for
  | 'break' ';'                                                   #break
  | 'continue' ';'                                                #continue
  | OPEN_CURLY stmt* CLOSE_CURLY                                  #stmtList
  ;

value
  : expr
  | STAR
  ;

augOp
  : '+=' | '-=' | '*=' | '&=' | '|=' | '^='
  ;

loopSpec
  : 'invariant' expr
  ;
