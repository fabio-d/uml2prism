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
	friend class SyntaxTree::GarbageCollectible;
	friend class Parser;

	public:
		enum SourceType
		{
			Script,		// a script (to define nodes' behaviors)
			Value		// a value (to initialize global variales)
		};

		SyntaxTreeGenerator(const QString &sourceCode, SourceType type);
		~SyntaxTreeGenerator();

		bool success() const;
		const SourceLocation &errorLocation() const;
		const QString &errorMessage() const;

		SyntaxTree::Statement *resultScript();
		SyntaxTree::Expression *resultValue();

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setResultScript(SyntaxTree::Statement *expr);
		void setResultValue(SyntaxTree::Expression *expr);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		SyntaxTree::Statement *m_resultScript;
		SyntaxTree::Expression *m_resultValue;

		QSet<SyntaxTree::GarbageCollectible*> m_allNodes;
};

}
}

#endif // CORE_COMPILER_SYNTAXTREEGENERATOR_H
