%{
#include "Core/ScriptLanguage/Lexer.h"

typedef Core::ScriptLanguage::Parser::token token;

#undef YY_DECL
#define YY_DECL \
	int Core::ScriptLanguage::Lexer::yylex( \
		Core::ScriptLanguage::Parser::semantic_type *const yylval, \
		Core::ScriptLanguage::Parser::location_type *loc)

#define YY_NO_UNISTD_H

#define YY_USER_ACTION \
	loc->step(); \
	loc->columns(yyleng);
%}

%option c++
%option nodefault
%option noyywrap
%option prefix="ScriptLanguage"
%option yyclass="Core::ScriptLanguage::Lexer"

alpha	[A-Za-z]
digit	[0-9]
identn	({alpha}|_)({alpha}|{digit}|_)*
number	[-+]?{digit}+
branchn	\"({alpha}|{digit}|_)*\"
ws	[ \t]+

%%

[0-9]+	{
		yylval->build<int>(atoi(yytext));
		return token::NUM;
	}

{ws}	/* skip whitespaces */

\n	{
		loc->lines();
		return token::LINE;
	}

.	{
		return yytext[0];
	}
%%
