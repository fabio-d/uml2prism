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

%skeleton	"lalr1.cc"
%require	"3.0"
%defines
%define		api.namespace {Core::Compiler}
%define		parser_class_name {Parser}
%define		api.value.type variant
%define		parse.assert
%define		parse.trace
%expect		2 // if-else and choice-or
%locations
%parse-param	{Lexer &lexer} {SyntaxTreeGenerator *owner} {bool allowProperties}

%code requires
{
	#include <QList>
	#include "Core/Compiler/SyntaxTree.h"

	namespace Core
	{
	namespace Compiler
	{
	class SyntaxTreeGenerator;
	class Lexer;
	}
	}
}

%code
{
	#include <iostream>
	#include <cstdlib>
	#include <fstream>

	#include "Core/Compiler/Lexer.h"
	#include "Core/Compiler/SyntaxTreeGenerator.h"

	#undef yylex
	#define yylex lexer.yylex

	static Core::Compiler::SourceLocation convertLocation(const Core::Compiler::location &l)
	{
		return Core::Compiler::SourceLocation(
			l.begin.line, l.begin.column,
			l.end.line, l.end.column > 0 ? (l.end.column - 1) : 0);
	}

	// All SyntaxTree classes' constructors take these two arguments in the
	// first two positions
	#define STDARGS \
		owner, \
		convertLocation(yylhs.location)

	// According to the type of expression that is being parsed, properties
	// might not be allowed
	#define CHECK_PROP_ALLOWED \
		do { \
			if (!allowProperties) \
				throw syntax_error(yylhs.location, "Property operators are not allowed in this context"); \
		} while(false)
}

%right		'='
%right		IMPLIES_OPERATOR IFF_OPERATOR
%left		AND_OPERATOR OR_OPERATOR
%left		EQUAL_OPERATOR NOT_EQUAL_OPERATOR
%precedence	'!'
%left		'.'

%token		<SyntaxTree::PropertyQuantifier> PROP_QUANTIF
%token		<SyntaxTree::UnaryProperty::Operator> PROP_UN_OP
%token		<SyntaxTree::BinaryProperty::Operator> PROP_BIN_OP
%token		<std::string> IDENTIFIER_SEGMENT
%token		<std::string> BRANCH_LABEL
%token		<bool> BOOL_LITERAL
%token		NIL_LITERAL

%token		BRANCH IF ELSE CHOICE OR

// Dummy tokens to select start symbol
%token		START_SCRIPT START_VALUE

%type		<SyntaxTree::Identifier*> ident
%type		<SyntaxTree::Expression*> literal
%type		<SyntaxTree::GlobalIdentifier*> signal
%type		<SyntaxTree::MethodCall*> method-call
%type		<SyntaxTree::Tuple*> tuple
%type		<QList<const SyntaxTree::Expression*>> expr-list
%type		<SyntaxTree::Expression*> expr
%type		<QList<const SyntaxTree::Statement*>> stmt-list
%type		<SyntaxTree::Statement*> stmt
%type		<SyntaxTree::IfElse*> if-else
%type		<SyntaxTree::ChoiceOr*> choice-or

%%

input:
  START_SCRIPT			{ owner->setResultScript(new SyntaxTree::CompoundStatement(STDARGS)); }
| START_SCRIPT stmt-list	{ owner->setResultScript(new SyntaxTree::CompoundStatement(STDARGS, $2)); }
| START_VALUE expr		{ owner->setResultValue($2); }
;

ident:
  IDENTIFIER_SEGMENT		{ $$ = new SyntaxTree::GlobalIdentifier(STDARGS, QString::fromStdString($1)); }
| ident '.' IDENTIFIER_SEGMENT	{ $$ = new SyntaxTree::MemberIdentifier(STDARGS, $1, QString::fromStdString($3)); }
;

literal:
  BOOL_LITERAL			{ $$ = new SyntaxTree::BoolLiteral(STDARGS, $1); }
| NIL_LITERAL			{ $$ = new SyntaxTree::NilLiteral(STDARGS); }
;

signal:
  IDENTIFIER_SEGMENT		{ $$ = new SyntaxTree::GlobalIdentifier(STDARGS, QString::fromStdString($1)); }
;

method-call:
  ident '(' ')'			{ $$ = new SyntaxTree::MethodCall(STDARGS, $1); }
| ident '(' expr-list ')'	{ $$ = new SyntaxTree::MethodCall(STDARGS, $1, $3); }
;

tuple:
  '{' '}'			{ $$ = new SyntaxTree::Tuple(STDARGS); }
| '{' expr-list '}'		{ $$ = new SyntaxTree::Tuple(STDARGS, $2); }
;

expr-list:
  expr				{ $$ = QList<const SyntaxTree::Expression*>() << $1; }
| expr-list ',' expr 		{ $$ = $1; $$ << $3; }
;

expr:
  ident				{ $$ = $1; }
| literal			{ $$ = $1; }
| method-call			{ $$ = $1; }
| '!' expr			{ $$ = new SyntaxTree::NotOperator(STDARGS, $2); }
| expr EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Equal, $1, $3); }
| expr NOT_EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::NotEqual, $1, $3); }
| expr AND_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::And, $1, $3); }
| expr OR_OPERATOR expr		{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Or, $1, $3); }
| expr IMPLIES_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Implies, $1, $3); }
| expr IFF_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Iff, $1, $3); }
| PROP_QUANTIF '[' PROP_UN_OP expr ']' { CHECK_PROP_ALLOWED; $$ = new SyntaxTree::UnaryProperty(STDARGS, $1, $3, $4); }
| PROP_QUANTIF '[' expr PROP_BIN_OP expr ']' { CHECK_PROP_ALLOWED; $$ = new SyntaxTree::BinaryProperty(STDARGS, $1, $4, $3, $5); }
| '(' expr ')'			{ $$ = $2; }
| tuple				{ $$ = $1; }
;

stmt-list:
  stmt				{ $$ = QList<const SyntaxTree::Statement*>() << $1; }
| stmt-list stmt 		{ $$ = $1; $$ << $2; }
;

stmt:
  ';'				{ $$ = new SyntaxTree::CompoundStatement(STDARGS); }
| ident '=' expr ';'		{ $$ = new SyntaxTree::Assignment(STDARGS, $1, $3); }
| BRANCH ';'			{ $$ = new SyntaxTree::Branch(STDARGS, "$default$"); }
| BRANCH BRANCH_LABEL ';'	{ $$ = new SyntaxTree::Branch(STDARGS, QString::fromStdString($2)); }
| method-call ';'		{ $$ = $1; }
| signal '!' ';'		{ $$ = new SyntaxTree::SignalEmission(STDARGS, $1); }
| signal '!' ident ';'		{ $$ = new SyntaxTree::SignalEmission(STDARGS, $1, $3); }
| signal '!' literal ';'	{ $$ = new SyntaxTree::SignalEmission(STDARGS, $1, $3); }
| signal '!' tuple ';' 		{ $$ = new SyntaxTree::SignalEmission(STDARGS, $1, $3); }
| signal '!' '(' expr ')' ';'	{ $$ = new SyntaxTree::SignalEmission(STDARGS, $1, $4); }
| '{' '}'			{ $$ = new SyntaxTree::CompoundStatement(STDARGS); }
| '{' stmt-list '}'		{ $$ = new SyntaxTree::CompoundStatement(STDARGS, $2); }
| if-else			{ $$ = $1; }
| choice-or			{ $$ = $1; }
;

if-else:
  IF '(' expr ')' stmt		{ $$ = new SyntaxTree::IfElse(STDARGS, $3, $5, new SyntaxTree::CompoundStatement(STDARGS)); }
| IF '(' expr ')' stmt ELSE stmt{ $$ = new SyntaxTree::IfElse(STDARGS, $3, $5, $7); }
;

choice-or:
  CHOICE stmt OR stmt		{ $$ = new SyntaxTree::ChoiceOr(STDARGS, $2, $4); }
| choice-or OR stmt		{ $$ = new SyntaxTree::ChoiceOr(STDARGS, $1, $3); }
;

%%
void Core::Compiler::Parser::error(const location_type &l, const std::string &err_message)
{
	owner->setError(convertLocation(l), QString::fromStdString(err_message));
}
