lexer grammar FlanLexer;

// Keywords
TYPE: 'type';
LANG: 'lang';
CONST: 'const';
METHOD: 'method';
REQUIRES: 'requires';
ENSURES: 'ensures';
VAR: 'var';
ASSUME: 'assume';
ASSERT: 'assert';
RETURN: 'return';
IF: 'if';
ELSE: 'else';
WHILE: 'while';
INVARIANT: 'invariant';
BREAK: 'break';
CONTINUE: 'continue';
FOR: 'for';
NULL: 'null';
TRUE: 'true';
FALSE: 'false';
LAMBDA: 'lambda';

// Delimiters
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
OPEN_SQUARE: '[';
CLOSE_SQUARE: ']';
OPEN_CURLY: '{';
CLOSE_CURLY: '}';
COMMA: ',';
SEMICOLON: ';';

// Specials
ASSIGN: '=';
COLON: ':';
ARROW: '->';
DOT: '.';
QUESTION: '?';

// Operators
AND: '&&';
OR: '||';
NOT: '!';
IMPLIES: '==>';
PLUS: '+';
MINUS: '-';
STAR: '*';
AUG_PLUS: '+=';
AUG_MINUS: '-=';
AUG_STAR: '*=';
EQ: '==';
NE: '!=';
LE: '<=';
LT: '<';
GE: '>=';
GT: '>';
IN: 'in';
SHL: '<<';
SHR: '>>';
BIT_AND: '&';
AUG_BIT_AND: '&=';
VERT: '|';
AUG_VERT: '|=';
BIT_XOR: '^';
AUG_BIT_XOR: '^=';
BIT_NOT: '~';

// Constants
INT: '-'? ([0-9]+ | '0x' HexDigit+ | '0b' [01]+);
fragment HexDigit: [0-9a-fA-F];

CHAR: '\'' ( ~('\'' | '\\' | '\r' | '\n') | '\\' CharEscape ) '\'';
STRING: '"' ( ~('"' | '\\' | '\r' | '\n') | '\\' CharEscape )* '"';

fragment CharEscape
  : [btnrf]
  | UnicodeEscape
  | '\'' | '"' | '\\'
  ;

fragment UnicodeEscape
  : 'x' HexDigit HexDigit
  | 'u' HexDigit HexDigit HexDigit HexDigit
  ;

// Identifiers
IDENT: IdentStart IdentPart*;
fragment IdentStart: [_a-zA-Z];
fragment IdentPart: IdentStart | [0-9];

// Whitespace and comments
WHITESPACE: [ \t\r\n\u000C]+ -> skip;
COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
LINE_COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);

// Formal languages
CC_BEGIN: 'r[' '^'? -> pushMode(CC);
RE_BEGIN: 'r"' -> pushMode(RE);

// Character classes
mode CC;
CC_END: ']' -> popMode;
CC_DASH: '-';
CC_CHAR: ~('[' | ']' | '-' | '\\' | '\r' | '\n') | '\\' RE_CharEscape;
CC_CHAR_TYPE: '\\' CC_CharTypeEscape;

fragment CC_CharTypeEscape
  : [pP] '{' [_a-zA-Z0-9]+ '}'
  ;

// Regular expressions
mode RE;
RE_END: '"' -> popMode;
RE_OPEN_PAREN: '(' -> type(OPEN_PAREN);
RE_CLOSE_PAREN: ')' -> type(CLOSE_PAREN);
RE_UNION: '|' -> type(VERT);
RE_STAR: '*' -> type(STAR);
RE_PLUS: '+' -> type(PLUS);
RE_OPT: '?' -> type(QUESTION);
RE_CHAR: RE_NormalChar | '\\' RE_CharEscape;
RE_CHAR_TYPE: '.' | '\\' CC_CharTypeEscape;

fragment RE_NormalChar
  : ~('"' | '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '\r' | '\n');

fragment RE_CharEscape
  : [tnrf]
  | 'c' [a-zA-Z]
  | UnicodeEscape
  | RE_SyntaxChar
  ;

fragment RE_SyntaxChar
  : '"' | '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '-'
  ;

RE_CC_BEGIN: '[' '^'? -> type(CC_BEGIN), pushMode(CC);
RE_QUANT_BEGIN: '{' -> type(OPEN_CURLY), pushMode(QUANT);

// RE Quantifiers
mode QUANT;
QUANT_END: '}' -> type(CLOSE_CURLY), popMode;
QUANT_COMMA: ',' -> type(COMMA);
QUANT_INT: [0-9]+ -> type(INT);
