#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H

#include "Core/ScriptLanguage/SyntaxTree.h"

#include <QSet>
#include <QString>

namespace Core
{
namespace ScriptLanguage
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

		SyntaxTree::Expression *takeResultScript();
		SyntaxTree::Expression *takeResultValue();

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setResultScript(SyntaxTree::Expression *expr);
		void setResultValue(SyntaxTree::Expression *expr);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		SyntaxTree::Expression *m_resultScript;
		SyntaxTree::Expression *m_resultValue;

		QSet<SyntaxTree::GarbageCollectible*> m_allNodes;
};

}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H
