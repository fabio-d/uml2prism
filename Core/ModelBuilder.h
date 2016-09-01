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

#ifndef CORE_MODELBUILDER_H
#define CORE_MODELBUILDER_H

#include "Core/Compiler/SemanticContext.h"

namespace Core
{

class DatatypeName;
class Document;
class UMLNodeElement;
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
		QString pctlPropertiesOutput() const;
		QString ctlPropertiesOutput() const;

	signals:
		void warning(const QString &location, const QString &description);
		void error(const QString &location, const QString &description);

	private:
		void emitWarning(const QString &location, const QString &description);
		void emitError(const QString &location, const QString &description);

		const Compiler::SemanticTree::Type *resolveType(const DatatypeName *dt) const;
		const Compiler::SemanticTree::Stmt *parseCustomScript(const UMLScriptedNodeElement *elem);
		const Compiler::SemanticTree::Stmt *generateDefaultScript(const UMLNodeElement *elem);

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
		QMap<QString, int> m_maxValueByState;

		QString m_modelOutput, m_propertiesOutput;

		bool m_verboseDebugEnabled;
};

}

#endif // CORE_MODELBUILDER_H
