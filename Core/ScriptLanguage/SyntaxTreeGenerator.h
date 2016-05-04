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
			Action,
			Expression
		};

		SyntaxTreeGenerator(const QString &sourceCode, SourceType type);

	private:
		void setError(int line, int column, const QString &message);
		void setResult(SyntaxTree::Expression *expr);

		bool m_success;
		int m_errorLine, m_errorColumn;
		QString m_errorMessage;
};

}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREEGENERATOR_H
