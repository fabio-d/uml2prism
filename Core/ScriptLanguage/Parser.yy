%skeleton	"lalr1.cc"
%require	"3.0"
%defines
%define		api.namespace {Core::ScriptLanguage}
%define		parser_class_name {Parser}
%define		api.value.type variant
%define		parse.assert
%define		parse.trace
%locations
%parse-param	{Lexer &lexer} {SyntaxTreeGenerator *owner}

%code requires
{
	namespace Core
	{
	namespace ScriptLanguage
	{
	class SyntaxTreeGenerator;
	class Lexer;

	namespace SyntaxTree
	{
	class Identifier;
	class Expression;
	class Tuple;
	}

	}
	}
}

%code
{
	#include <iostream>
	#include <cstdlib>
	#include <fstream>

	#include "Core/ScriptLanguage/Lexer.h"
	#include "Core/ScriptLanguage/SyntaxTreeGenerator.h"

	#undef yylex
	#define yylex lexer.yylex
}

%right		'='
%left		AND_OPERATOR OR_OPERATOR
%left		EQUAL_OPERATOR NOT_EQUAL_OPERATOR
%precedence	'!'
%left		'.'

%token		<std::string> IDENTIFIER_SEGMENT
%token		<bool> BOOL_LITERAL

// Dummy tokens to select start symbol
%token		START_SCRIPT
%token		START_VALUE

%type		<SyntaxTree::Identifier*> ident
%type		<SyntaxTree::Tuple*> expr-list
%type		<SyntaxTree::Expression*> expr

%%

input:
  START_SCRIPT expr		{ owner->setResultScript($2); }
| START_VALUE expr		{ owner->setResultValue($2); }
;

ident:
  IDENTIFIER_SEGMENT		{ $$ = new SyntaxTree::GlobalIdentifier(owner, QString::fromStdString($1)); }
| ident '.' IDENTIFIER_SEGMENT	{ $$ = new SyntaxTree::MemberIdentifier(owner, $1, QString::fromStdString($3)); }
;

expr-list:
  expr				{ $$ = new SyntaxTree::Tuple(owner); $$->appendElement($1); }
| expr-list ',' expr 		{ $$ = $1; $1->appendElement($3); }
;

expr:
  BOOL_LITERAL			{ $$ = new SyntaxTree::BoolLiteral(owner, $1); }
| ident				{ $$ = $1; }
| ident '(' ')'			{ $$ = new SyntaxTree::MethodCall(owner, $1); }
| ident '(' expr-list ')'	{ $$ = new SyntaxTree::MethodCall(owner, $1, $3); }
| '!' expr			{ $$ = new SyntaxTree::NotOperator(owner, $2); }
| expr EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(owner, SyntaxTree::BinaryOperator::Equal, $1, $3); }
| expr NOT_EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(owner, SyntaxTree::BinaryOperator::NotEqual, $1, $3); }
| expr AND_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(owner, SyntaxTree::BinaryOperator::And, $1, $3); }
| expr OR_OPERATOR expr		{ $$ = new SyntaxTree::BinaryOperator(owner, SyntaxTree::BinaryOperator::Or, $1, $3); }
| '(' expr ')'			{ $$ = $2; }
| '{' '}'			{ $$ = new SyntaxTree::Tuple(owner); }
| '{' expr-list '}'		{ $$ = $2; }
;

%%
void Core::ScriptLanguage::Parser::error(const location_type &l, const std::string &err_message)
{
	owner->setError(l.begin.line, l.begin.column, QString::fromStdString(err_message));
	std::cerr << "Error: " << err_message << " at " << l << "\n";
}
