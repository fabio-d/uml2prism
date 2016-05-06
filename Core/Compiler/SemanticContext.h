#ifndef CORE_COMPILER_SEMANTICCONTEXT_H
#define CORE_COMPILER_SEMANTICCONTEXT_H

#include <QMap>
#include <QSet>
#include <QStringList>

namespace Core
{
namespace Compiler
{

class SemanticContext
{
	public:
		class Type
		{
			public:
				virtual const QString datatypeName() const = 0;
				virtual void fillReferencedTypes(QSet<const Type*> &target) const;
		};

		class BoolType : public Type
		{
			public:
				const QString datatypeName() const override;
		};

		class EnumerationType : public Type
		{
			public:
				explicit EnumerationType(const QString &datatypeName);

				void registerValue(const QString &valueName);

				const QString datatypeName() const override;

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

			private:
				QString m_datatypeName;
				QMap<QString, const Type*> m_memberVariables;
		};

		class SetType : public Type
		{
			public:
				explicit SetType(const Type *innerType);

				const QString datatypeName() const override;
				void fillReferencedTypes(QSet<const Type*> &target) const override;

			private:
				const Type *m_innerType;
		};

		SemanticContext();
		~SemanticContext();

		const BoolType *boolType() const;
		const Type *findOtherType(const QString &name) const;
		const SetType *findSetType(const Type *innerType) const;

		EnumerationType *registerEnumeration(const QString &enumName);
		ClassType *registerClass(const QString &className);
		void registerGlobalVariable(const QString &name, const Type *type);
		void registerSignal(const QString &name, const Type *type); // type==nullptr means no attached message

	private:
		QMap<QString, const Type*> m_classAndEnumTypes; // EnumerationType and ClassType instances by name
		mutable QMap<const Type*, const SetType*> m_setTypes; // SetType instances by innerType
		QMap<QString, const Type*> m_globalVariables;
		QMap<QString, const Type*> m_signals;
};

}
}

#endif // CORE_COMPILER_SEMANTICCONTEXT_H
