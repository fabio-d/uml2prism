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
	#include <QList>

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
	class MethodCall;
	class Statement;
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

	static Core::ScriptLanguage::SourceLocation convertLocation(const Core::ScriptLanguage::location &l)
	{
		return Core::ScriptLanguage::SourceLocation(
			l.begin.line, l.begin.column,
			l.end.line, l.end.column > 0 ? (l.end.column - 1) : 0);
	}

	// All SyntaxTree classes' constructors take these two arguments in the
	// first two positions
	#define STDARGS \
		owner, \
		convertLocation(yylhs.location)
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
%type		<SyntaxTree::MethodCall*> method-call
%type		<QList<SyntaxTree::Expression*>> expr-list
%type		<SyntaxTree::Expression*> expr
%type		<QList<SyntaxTree::Statement*>> stmt-list
%type		<SyntaxTree::Statement*> stmt

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

method-call:
  ident '(' ')'			{ $$ = new SyntaxTree::MethodCall(STDARGS, $1); }
| ident '(' expr-list ')'	{ $$ = new SyntaxTree::MethodCall(STDARGS, $1, $3); }
;

expr-list:
  expr				{ $$ = QList<SyntaxTree::Expression*>() << $1; }
| expr-list ',' expr 		{ $$ = $1; $$ << $3; }
;

expr:
  BOOL_LITERAL			{ $$ = new SyntaxTree::BoolLiteral(STDARGS, $1); }
| ident				{ $$ = $1; }
| method-call			{ $$ = $1; }
| '!' expr			{ $$ = new SyntaxTree::NotOperator(STDARGS, $2); }
| expr EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Equal, $1, $3); }
| expr NOT_EQUAL_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::NotEqual, $1, $3); }
| expr AND_OPERATOR expr	{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::And, $1, $3); }
| expr OR_OPERATOR expr		{ $$ = new SyntaxTree::BinaryOperator(STDARGS, SyntaxTree::BinaryOperator::Or, $1, $3); }
| '(' expr ')'			{ $$ = $2; }
| '{' '}'			{ $$ = new SyntaxTree::Tuple(STDARGS); }
| '{' expr-list '}'		{ $$ = new SyntaxTree::Tuple(STDARGS, $2); }
;

stmt-list:
  stmt				{ $$ = QList<SyntaxTree::Statement*>() << $1; }
| stmt-list stmt 		{ $$ = $1; $$ << $2; }
;

stmt:
  ';'				{ $$ = new SyntaxTree::CompoundStatement(STDARGS); }
| method-call ';'		{ $$ = $1; }
| '{' '}'			{ $$ = new SyntaxTree::CompoundStatement(STDARGS); }
| '{' stmt-list '}'		{ $$ = new SyntaxTree::CompoundStatement(STDARGS, $2); }
;

%%
void Core::ScriptLanguage::Parser::error(const location_type &l, const std::string &err_message)
{
	owner->setError(convertLocation(l), QString::fromStdString(err_message));
}
