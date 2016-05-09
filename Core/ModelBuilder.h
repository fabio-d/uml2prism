#ifndef CORE_MODELBUILDER_H
#define CORE_MODELBUILDER_H

#include "Core/Compiler/SemanticContext.h"

namespace Core
{

class DatatypeName;
class Document;

class ModelBuilder
{
	public:
		explicit ModelBuilder(const Document *doc);
		~ModelBuilder();

		bool success() const;
		const Compiler::SemanticContext *semanticContext() const;

	private:
		void emitWarning(const QString &location, const QString &description);
		void emitError(const QString &location, const QString &description);

		const Compiler::SemanticTree::Type *resolveType(const DatatypeName *dt) const;

		void checkDuplicateGlobalNames();
		void checkControlFlowEdges();
		void checkSignalEdges();
		void registerTypes();
		void registerGlobalVariables();
		void registerSignals();
		void registerStates();
		void compileVariableDecls();

		const Document *m_doc;
		bool m_error;

		Compiler::SemanticContext m_semanticContext;
		QMap<QString, const Compiler::SemanticTree::Expr*> m_globalVarsInitValue;
		QList<QString> m_persistentVariables;
};

}

#endif // CORE_MODELBUILDER_H
