/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

%{
#include "Core/Compiler/Lexer.h"
#include "Core/Compiler/SyntaxTree.h"

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

A	{
		yylval->build<SyntaxTree::PropertyQuantifier>(SyntaxTree::PropertyQuantifier::ForAll);
		return token::PROP_QUANTIF;
	}

E	{
		yylval->build<SyntaxTree::PropertyQuantifier>(SyntaxTree::PropertyQuantifier::Exists);
		return token::PROP_QUANTIF;
	}

X	{
		yylval->build<SyntaxTree::UnaryProperty::Operator>(SyntaxTree::UnaryProperty::Operator::Next);
		return token::PROP_UN_OP;
	}

F	{
		yylval->build<SyntaxTree::UnaryProperty::Operator>(SyntaxTree::UnaryProperty::Operator::Eventually);
		return token::PROP_UN_OP;
	}

G	{
		yylval->build<SyntaxTree::UnaryProperty::Operator>(SyntaxTree::UnaryProperty::Operator::Always);
		return token::PROP_UN_OP;
	}

U	{
		yylval->build<SyntaxTree::BinaryProperty::Operator>(SyntaxTree::BinaryProperty::Operator::Until);
		return token::PROP_BIN_OP;
	}

W	{
		yylval->build<SyntaxTree::BinaryProperty::Operator>(SyntaxTree::BinaryProperty::Operator::WeakUntil);
		return token::PROP_BIN_OP;
	}

R	{
		yylval->build<SyntaxTree::BinaryProperty::Operator>(SyntaxTree::BinaryProperty::Operator::Release);
		return token::PROP_BIN_OP;
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
