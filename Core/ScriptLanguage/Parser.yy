%skeleton	"lalr1.cc"
%require	"3.0"
%defines
%define		api.namespace {Core::ScriptLanguage}
%define		parser_class_name {Parser}
%define		api.value.type variant
%define		parse.assert
%define		parse.trace

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
	class AddOp;
	class Constant;
	class Expression;
	}

	}
	}
}

%parse-param	{Lexer &lexer} {SyntaxTreeGenerator *owner}

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

%locations

/* Bison declarations.  */
%token END 0 /* eof */

%token		<int> NUM
%left		'-' '+'
%left		'*' '/'
%precedence	NEG /* negation--unary minus */
%right		'^' /* exponentiation */

%token		LINE
%type		<SyntaxTree::Expression*> exp

%% /* The grammar follows.  */

input:
  %empty
| input line
;

line:
  LINE
| exp LINE  { puts($1->toString().toLatin1().constData()); }
;

exp:
  NUM                { $$ = new SyntaxTree::Constant($1); }
| exp '+' exp        { $$ = new SyntaxTree::AddOp($1, $3); }
//| exp '-' exp        { $$ = $1 - $3;      }
//| exp '*' exp        { $$ = $1 * $3;      }
//| exp '/' exp        { $$ = $1 / $3;      }
//| '-' exp  %prec NEG { $$ = -$2;          }
//| exp '^' exp        { $$ = pow ($1, $3); }
| '(' exp ')'        { $$ = $2;           }
;

%%
void Core::ScriptLanguage::Parser::error(const location_type &l, const std::string &err_message)
{
	owner->setError(l.begin.line, l.begin.column, QString::fromStdString(err_message));
	std::cerr << "Error: " << err_message << " at " << l << "\n";
}
