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

void Core::ScriptLanguage::Lexer::injectToken(int token)
{
	m_tokenToInjectIsPresent = true;
	m_tokenToInject = token;
}
%}

%option c++
%option nodefault
%option noyywrap
%option prefix="ScriptLanguage"
%option yyclass="Core::ScriptLanguage::Lexer"

alpha	[A-Za-z]
digit	[0-9]
identn	({alpha}|_)({alpha}|{digit}|_)*
branchn	\"({alpha}|{digit}|_)*\"
ws	[ \t]+

%%

%{
	if (m_tokenToInjectIsPresent)
	{
		m_tokenToInjectIsPresent = false;
		return m_tokenToInject;
	}
%}

true	{
		yylval->build<bool>(true);
		return token::BOOL_LITERAL;
	}

false	{
		yylval->build<bool>(false);
		return token::BOOL_LITERAL;
	}

{identn} {
		yylval->build<std::string>(yytext);
		return token::IDENTIFIER_SEGMENT;
	}

==	{
		return token::EQUAL_OPERATOR;
	}

!=	{
		return token::NOT_EQUAL_OPERATOR;
	}

&&	{
		return token::AND_OPERATOR;
	}

\|\|	{
		return token::OR_OPERATOR;
	}

{ws}	/* skip whitespaces */

\n	{
		loc->lines();
	}

.	{
		return yytext[0];
	}
%%
