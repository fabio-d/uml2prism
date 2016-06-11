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
