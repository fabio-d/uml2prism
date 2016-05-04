#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H

#include "Core/ScriptLanguage/SyntaxTree.h"

#include <QString>

namespace Core
{
namespace ScriptLanguage
{

class SyntaxTreeGenerator
{
	friend class Parser;

	public:
		enum SourceType
		{
			Script,		// a script (to define nodes' behaviors)
			Value		// a value (to initialize global variales)
		};

		SyntaxTreeGenerator(const QString &sourceCode, SourceType type);

	private:
		void setError(int line, int column, const QString &message);
		void setResultScript(SyntaxTree::Expression *expr);
		void setResultValue(SyntaxTree::Expression *expr);

		bool m_success;
		int m_errorLine, m_errorColumn;
		QString m_errorMessage;

		SyntaxTree::Expression *m_resultScript;
		SyntaxTree::Expression *m_resultValue;
};

}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H
