#ifndef CORE_MODELBUILDER_H
#define CORE_MODELBUILDER_H

#include "Core/Compiler/SemanticContext.h"

namespace Core
{

class DatatypeName;
class Document;
class UMLScriptedNodeElement;

class ModelBuilder : public QObject
{
	Q_OBJECT

	public:
		ModelBuilder(const Document *doc, bool debugOutput); // does NOT take ownership
		~ModelBuilder();

		bool run(); // to called only once, returns same value as success()
		bool success() const;
		const Compiler::SemanticContext *semanticContext() const;

		const QString &modelOutput() const;
		const QString &propertiesOutput() const;

	signals:
		void warning(const QString &location, const QString &description);
		void error(const QString &location, const QString &description);

	private:
		void emitWarning(const QString &location, const QString &description);
		void emitError(const QString &location, const QString &description);

		const Compiler::SemanticTree::Type *resolveType(const DatatypeName *dt) const;
		const Compiler::SemanticTree::Stmt *parseCustomScript(const UMLScriptedNodeElement *elem);

		void checkDuplicateGlobalNames();
		void checkControlFlowEdges();
		void checkSignalEdges();
		void registerTypes();
		void registerGlobalVariables();
		void registerSignals();
		void registerStates();
		void registerLabels();
		QString compileVariableDecls();
		QString compileSignalDecls();
		QString compileStates();
		QString compileLabels();
		QString compileProperties();

		const Document *m_doc;
		bool m_started, m_error;

		Compiler::SemanticContext m_semanticContext;
		QMap<QString, const Compiler::SemanticTree::Expr*> m_globalVarsInitValue;
		QList<QString> m_persistentVariables;

		QString m_modelOutput, m_propertiesOutput;

		bool m_verboseDebugEnabled;
};

}

#endif // CORE_MODELBUILDER_H
