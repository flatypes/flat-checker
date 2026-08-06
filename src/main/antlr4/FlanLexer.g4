lexer grammar FlanLexer;

// Keywords
TYPE: 'type';
LANG: 'lang';
CONST: 'const';
METHOD: 'method';
RETURNS: 'returns';
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
ABORT: 'abort';
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
AND: 'and' | '&&';
OR: 'or' | '||';
NOT: 'not' | '!';
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
IN_LANG: '∈';
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
INT_LITERAL: '-'? ([0-9]+ | '0x' HexDigit+ | '0b' [01]+);
fragment HexDigit: [0-9a-fA-F];

CHAR_LITERAL: '\'' ( ~('\'' | '\\' | '\r' | '\n') | '\\' CharEscape ) '\'';
STRING_LITERAL: '"' ( ~('"' | '\\' | '\r' | '\n') | '\\' CharEscape )* '"';
REGEX_LITERAL: 'r"' ( ~('"' | '\\' | '\r' | '\n') | '\\' CharEscape )* '"';

fragment CharEscape
  : ~[\r\n]
  ;

// Identifiers
IDENT: IdentStart IdentPart*;
fragment IdentStart: [_a-zA-Z];
fragment IdentPart: IdentStart | [0-9];

// Whitespace and comments
WHITESPACE: [ \t\r\n\u000C]+ -> skip;
COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
LINE_COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);
