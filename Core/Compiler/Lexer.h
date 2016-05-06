#ifndef CORE_COMPILER_LEXER_H
#define CORE_COMPILER_LEXER_H

#ifndef yyFlexLexerOnce
# undef yyFlexLexer
# define yyFlexLexer CompilerFlexLexer
# include <FlexLexer.h>
# undef yyFlexLexer
#endif

#include "Core/Compiler/Parser.h"

namespace Core
{
namespace Compiler
{

class Lexer : public CompilerFlexLexer
{
	public:
		Lexer(std::istream *in)
		: CompilerFlexLexer(in)
		{
		}

		void injectToken(int token);

		int yylex(Parser::semantic_type *const lval,
			  Parser::location_type *location);

	private:
		bool m_tokenToInjectIsPresent;
		int m_tokenToInject;
};

}
}

#endif // CORE_COMPILER_LEXER_H
