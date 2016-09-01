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

#ifndef CORE_COMPILER_SYNTAXTREEGENERATOR_H
#define CORE_COMPILER_SYNTAXTREEGENERATOR_H

#include "Core/Compiler/SyntaxTree.h"

#include <QSet>
#include <QString>

namespace Core
{
namespace Compiler
{

class SyntaxTreeGenerator
{
	friend class SyntaxTree::Node;
	friend class Parser;

	public:
		enum SourceType
		{
			Script,		// a script (to define nodes' behaviors)
			Value,		// a value (a label or to initialize global variales)
			Property	// a properties to be verified
		};

		SyntaxTreeGenerator(const QString &sourceCode, SourceType type);
		~SyntaxTreeGenerator();

		bool success() const;
		const SourceLocation &errorLocation() const;
		const QString &errorMessage() const;

		SyntaxTree::Statement *resultScript();
		SyntaxTree::Expression *resultValue(); // for properties too

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setResultScript(SyntaxTree::Statement *expr);
		void setResultValue(SyntaxTree::Expression *expr);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		SyntaxTree::Statement *m_resultScript;
		SyntaxTree::Expression *m_resultValue;

		QSet<SyntaxTree::Node*> m_allNodes;
};

}
}

#endif // CORE_COMPILER_SYNTAXTREEGENERATOR_H
