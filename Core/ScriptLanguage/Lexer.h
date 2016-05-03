#ifndef CORE_SCRIPTLANGUAGE_LEXER_H
#define CORE_SCRIPTLANGUAGE_LEXER_H

#ifndef yyFlexLexerOnce
# undef yyFlexLexer
# define yyFlexLexer ScriptLanguageFlexLexer
# include <FlexLexer.h>
# undef yyFlexLexer
#endif

#include "Core/ScriptLanguage/Parser.h"

namespace Core
{
namespace ScriptLanguage
{

class Lexer : public ScriptLanguageFlexLexer
{
	public:
		Lexer(std::istream *in)
		: ScriptLanguageFlexLexer(in)
		{
		}

		int yylex(Parser::semantic_type *const lval,
			  Parser::location_type *location);
};

}
}

#endif // CORE_SCRIPTLANGUAGE_LEXER_H
