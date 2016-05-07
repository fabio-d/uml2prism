#ifndef CORE_COMPILER_SEMANTICTREE_H
#define CORE_COMPILER_SEMANTICTREE_H

#include "Core/Compiler/Bindings_stub.h"

#include <QSet>
#include <QStringList>

namespace Core
{
namespace Compiler
{
namespace SemanticTree
{

class Type
{
	public:
		virtual ~Type();

		virtual const QString datatypeName() const = 0;
		virtual void fillReferencedTypes(QSet<const Type*> &target) const;

		HsStablePtr haskellHandle() const;

	protected:
		Type();
		virtual HsStablePtr createHaskellHandle() const = 0;
		bool hasHaskellHandleBeenCreated() const;

	private:
		mutable HsStablePtr m_haskellHandle;
};

class BoolType : public Type
{
	public:
		const QString datatypeName() const override;

	protected:
		HsStablePtr createHaskellHandle() const override;
};

class EnumerationType : public Type
{
	public:
		explicit EnumerationType(const QString &datatypeName);

		void registerValue(const QString &valueName);

		const QString datatypeName() const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		QString m_datatypeName;
		QStringList m_values;
};

class ClassType : public Type
{
	public:
		explicit ClassType(const QString &datatypeName);

		void registerMemberVariable(const QString &variableName, const Type *type);

		const QString datatypeName() const override;
		void fillReferencedTypes(QSet<const Type*> &target) const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		QString m_datatypeName;
		QList<QPair<QString, const Type*>> m_memberVariables;
};

class SetType : public Type
{
	public:
		explicit SetType(const Type *innerType);

		const QString datatypeName() const override;
		void fillReferencedTypes(QSet<const Type*> &target) const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		const Type *m_innerType;
};

}
}
}

#endif // CORE_COMPILER_SEMANTICTREE_H
