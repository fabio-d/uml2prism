%{
#include "Core/Compiler/Lexer.h"

typedef Core::Compiler::Parser::token token;

#undef YY_DECL
#define YY_DECL \
	int Core::Compiler::Lexer::yylex( \
		Core::Compiler::Parser::semantic_type *const yylval, \
		Core::Compiler::Parser::location_type *loc)

#define YY_NO_UNISTD_H

#define YY_USER_ACTION \
	loc->step(); \
	loc->columns(yyleng);

void Core::Compiler::Lexer::injectToken(int token)
{
	m_tokenToInjectIsPresent = true;
	m_tokenToInject = token;
}
%}

%option c++
%option nodefault
%option noyywrap
%option prefix="Compiler"
%option yyclass="Core::Compiler::Lexer"

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

nil	{
		return token::NIL_LITERAL;
	}

branch	{
		return token::BRANCH;
	}

if	{
		return token::IF;
	}

else	{
		return token::ELSE;
	}

choice	{
		return token::CHOICE;
	}

or	{
		return token::OR;
	}

{branchn} {
		const std::string text_with_quotes(yytext);
		const std::string text_without_quotes(text_with_quotes.substr(1, text_with_quotes.length() - 2));
		yylval->build<std::string>(text_without_quotes);
		return token::BRANCH_LABEL;
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

=>	{
		return token::IMPLIES_OPERATOR;
	}

\<=>	{
		return token::IFF_OPERATOR;
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
