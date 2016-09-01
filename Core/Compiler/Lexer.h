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
