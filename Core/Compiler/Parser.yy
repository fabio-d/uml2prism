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
%parse-param	{Lexer &lexer} {SyntaxTreeGenerator *owner}

%code requires
{
	#include <QList>

	namespace Core
	{
	namespace Compiler
	{
	class SyntaxTreeGenerator;
	class Lexer;

	namespace SyntaxTree
	{
	class ChoiceOr;
	class GlobalIdentifier;
	class Identifier;
	class IfElse;
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
}

%right		'='
%left		AND_OPERATOR OR_OPERATOR
%left		EQUAL_OPERATOR NOT_EQUAL_OPERATOR
%precedence	'!'
%left		'.'

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
%type		<QList<SyntaxTree::Expression*>> expr-list
%type		<SyntaxTree::Expression*> expr
%type		<QList<SyntaxTree::Statement*>> stmt-list
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
  expr				{ $$ = QList<SyntaxTree::Expression*>() << $1; }
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
| '(' expr ')'			{ $$ = $2; }
| tuple				{ $$ = $1; }
;

stmt-list:
  stmt				{ $$ = QList<SyntaxTree::Statement*>() << $1; }
| stmt-list stmt 		{ $$ = $1; $$ << $2; }
;

stmt:
  ';'				{ $$ = new SyntaxTree::CompoundStatement(STDARGS); }
| ident '=' expr ';'		{ $$ = new SyntaxTree::Assignment(STDARGS, $1, $3); }
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