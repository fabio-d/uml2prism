#include "Core/Compiler/SemanticTree.h"

namespace Core
{
namespace Compiler
{
namespace SemanticTree
{

Type::Type()
: m_haskellHandle(nullptr)
{
}

Type::~Type()
{
	if (m_haskellHandle != nullptr)
		hsType_free(m_haskellHandle);
}

void Type::fillReferencedTypes(QSet<const Type*> &target) const
{
}

bool Type::hasHaskellHandleBeenCreated() const
{
	return m_haskellHandle != nullptr;
}

HsStablePtr Type::haskellHandle() const
{
	if (m_haskellHandle == nullptr)
		m_haskellHandle = createHaskellHandle();
	return m_haskellHandle;
}

const QString BoolType::datatypeName() const
{
	return "bool";
}

HsStablePtr BoolType::createHaskellHandle() const
{
	return hsTypeBool_create();
}

EnumerationType::EnumerationType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void EnumerationType::registerValue(const QString &valueName)
{
	Q_ASSERT(hasHaskellHandleBeenCreated() == false);
	m_values.append(valueName);
}

const QStringList &EnumerationType::values() const
{
	return m_values;
}

const QString EnumerationType::datatypeName() const
{
	return m_datatypeName;
}

HsStablePtr EnumerationType::createHaskellHandle() const
{
	HsStablePtr res = hsTypeEnumeration_create();
	int i = m_values.count();
	while (i-- != 0)
	{
		const QByteArray valArr = m_values[i].toLatin1();
		HsStablePtr updated = hsTypeEnumeration_prependValue(res, (void*)valArr.constData());
		hsType_free(res);
		res = updated;
	}
	return res;
}

ClassType::ClassType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void ClassType::registerMemberVariable(const QString &variableName, const Type *type)
{
	Q_ASSERT(hasHaskellHandleBeenCreated() == false);
	m_memberVariables.append(QPair<QString, const Type*>(variableName, type));
}

QStringList ClassType::memberVariables() const
{
	QStringList res;
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		res.append(it->first);
	}
	return res;
}

const Type *ClassType::findMemberVariable(const QString &name) const
{
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		if (it->first == name)
			return it->second;
	}
	return nullptr;
}

const QString ClassType::datatypeName() const
{
	return m_datatypeName;
}

void ClassType::fillReferencedTypes(QSet<const Type*> &target) const
{
	QSet<const Type*> res;
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		const Type *t = it->second;
		if (target.contains(t))
			continue;

		target.insert(t);
		t->fillReferencedTypes(target);
	}
}

HsStablePtr ClassType::createHaskellHandle() const
{
	HsStablePtr res = hsTypeClass_create();
	int i = m_memberVariables.count();
	while (i-- != 0)
	{
		const QPair<QString, const Type*> &var = m_memberVariables[i];
		const QByteArray n = var.first.toLatin1();
		const Type *t = var.second;
		HsStablePtr updated = hsTypeClass_prependMemberVariable(res, (void*)n.constData(), t->haskellHandle());
		hsType_free(res);
		res = updated;
	}
	return res;
}

SetType::SetType(const Type *innerType)
: m_innerType(innerType)
{
}

const Type *SetType::innerType() const
{
	return m_innerType;
}

const QString SetType::datatypeName() const
{
	return QString("set of %1").arg(m_innerType->datatypeName());
}

void SetType::fillReferencedTypes(QSet<const Type*> &target) const
{
	target.insert(m_innerType);
	m_innerType->fillReferencedTypes(target);
}

HsStablePtr SetType::createHaskellHandle() const
{
	return hsTypeSet_create(m_innerType->haskellHandle());
}

Idnt::Idnt(const Type *type)
: m_haskellHandle(nullptr), m_type(type)
{
}

Idnt::~Idnt()
{
	hsIdnt_free(m_haskellHandle);
}

HsStablePtr Idnt::haskellHandle() const
{
	return m_haskellHandle;
}

const Type *Idnt::type() const
{
	return m_type;
}

IdntGlobal::IdntGlobal(const QString &name, const Type *type)
: Idnt(type), m_name(name)
{
	const QByteArray n = name.toLatin1();
	m_haskellHandle = hsIdntGlobal_create((void*)n.constData(), type->haskellHandle());
}

QString IdntGlobal::toString() const
{
	return m_name;
}

IdntMember::IdntMember(const Idnt *container, const QString &name, const Type *type)
: Idnt(type), m_container(container), m_name(name)
{
	const QByteArray n = name.toLatin1();
	m_haskellHandle = hsIdntMember_create(container->haskellHandle(),
		(void*)n.constData(), type->haskellHandle());
}

IdntMember::~IdntMember()
{
	delete m_container;
}

QString IdntMember::toString() const
{
	return QString("%1.%2").arg(m_container->toString()).arg(m_name);
}

Expr::Expr()
: m_haskellHandle(nullptr)
{
}

Expr::~Expr()
{
	hsExpr_free(m_haskellHandle);
}

HsStablePtr Expr::haskellHandle() const
{
	return m_haskellHandle;
}

ExprBoolLiteral::ExprBoolLiteral(bool value)
: m_value(value)
{
	m_haskellHandle = hsExprBoolLiteral_create(value);
}

QString ExprBoolLiteral::toString() const
{
	return QString("ExprBoolLiteral(%1)").arg(m_value ? "true" : "false");
}

ExprEnumLiteral::ExprEnumLiteral(const EnumerationType *type)
: m_type(type)
{
	m_haskellHandle = hsExprEnumLiteral_create(type->haskellHandle(), 0);
}

ExprEnumLiteral::ExprEnumLiteral(const EnumerationType *type, const QString &value)
: m_type(type), m_value(value)
{
	m_haskellHandle = hsExprEnumLiteral_create(type->haskellHandle(),
		1 + type->values().indexOf(value));
}

QString ExprEnumLiteral::toString() const
{
	if (m_value.isEmpty())
		return QString("ExprEnumLiteral(%1, nil)").arg(m_type->datatypeName());
	else
		return QString("ExprEnumLiteral(%1, %2)").arg(m_type->datatypeName()).arg(m_value);
}

ExprClassNilLiteral::ExprClassNilLiteral(const ClassType *type)
: m_type(type)
{
	m_haskellHandle = hsExprClassNilLiteral_create(type->haskellHandle());
}

QString ExprClassNilLiteral::toString() const
{
	return QString("ExprClassNilLiteral(%1)").arg(m_type->datatypeName());
}

ExprVariable::ExprVariable(const Idnt *identifier)
: m_identifier(identifier)
{
	m_haskellHandle = hsExprVariable_create(identifier->haskellHandle());
}

ExprVariable::~ExprVariable()
{
	delete m_identifier;
}

QString ExprVariable::toString() const
{
	return QString("ExprVariable(%1)").arg(m_identifier->toString());
}

ExprBinOp::ExprBinOp(Operator op, const Expr *arg1, const Expr *arg2)
: m_op(op), m_arg1(arg1), m_arg2(arg2)
{
	switch (m_op)
	{
		case Equal:
			m_haskellHandle = hsExprEqOp_create(arg1->haskellHandle(), arg2->haskellHandle());
			break;
		case NotEqual:
			m_haskellHandle = hsExprNeqOp_create(arg1->haskellHandle(), arg2->haskellHandle());
			break;
		case And:
			m_haskellHandle = hsExprAndOp_create(arg1->haskellHandle(), arg2->haskellHandle());
			break;
		case Or:
			m_haskellHandle = hsExprOrOp_create(arg1->haskellHandle(), arg2->haskellHandle());
			break;
	}
}

ExprBinOp::~ExprBinOp()
{
	delete m_arg1;
	delete m_arg2;
}

QString ExprBinOp::toString() const
{
	QString opStr;

	switch (m_op)
	{
		case Equal:
			opStr = "Equal";
			break;
		case NotEqual:
			opStr = "NotEqual";
			break;
		case And:
			opStr = "And";
			break;
		case Or:
			opStr = "Or";
			break;
	}

	return QString("ExprBinOp(%1, %2, %3)")
		.arg(opStr)
		.arg(m_arg1->toString())
		.arg(m_arg2->toString());
}

ExprNotOp::ExprNotOp(const Expr *arg)
: m_arg(arg)
{
	m_haskellHandle = hsExprNotOp_create(arg->haskellHandle());
}

ExprNotOp::~ExprNotOp()
{
	delete m_arg;
}

QString ExprNotOp::toString() const
{
	return QString("ExprNotOp(%1)")
		.arg(m_arg->toString());
}

ExprTuple::ExprTuple(const Type *type, const QList<const Expr*> &args)
: m_type(type), m_args(args)
{
	m_haskellHandle = hsExprTuple_create(type->haskellHandle());
	int i = args.count();
	while (i-- != 0)
	{
		HsStablePtr updated = hsExprTuple_prependTerm(m_haskellHandle, args[i]->haskellHandle());
		hsExpr_free(m_haskellHandle);
		m_haskellHandle = updated;
	}
}

ExprTuple::~ExprTuple()
{
	qDeleteAll(m_args);
}

QString ExprTuple::toString() const
{
	QStringList l;

	l.append(m_type->datatypeName());

	foreach (const Expr *e, m_args)
		l.append(e->toString());

	return QString("ExprTuple(%1)").arg(l.join(", "));
}

ExprSetContains::ExprSetContains(const Idnt *setIdentifier, const Expr *elementToTest)
: m_setIdentifier(setIdentifier), m_elementToTest(elementToTest)
{
	m_haskellHandle = hsExprSetContains_create(setIdentifier->haskellHandle(),
		elementToTest->haskellHandle());
}

ExprSetContains::~ExprSetContains()
{
	delete m_setIdentifier;
	delete m_elementToTest;
}

QString ExprSetContains::toString() const
{
	return QString("ExprSetContains(%1, %2)")
		.arg(m_setIdentifier->toString())
		.arg(m_elementToTest->toString());
}

Stmt::Stmt()
: m_haskellHandle(nullptr)
{
}

Stmt::~Stmt()
{
	hsStmt_free(m_haskellHandle);
}

HsStablePtr Stmt::haskellHandle() const
{
	return m_haskellHandle;
}

StmtCompound::StmtCompound(const QList<const Stmt*> &statements)
: m_statements(statements)
{
	m_haskellHandle = hsStmtCompound_create();
	int i = statements.count();
	while (i-- != 0)
	{
		HsStablePtr updated = hsStmtCompound_prependStatement(m_haskellHandle, statements[i]->haskellHandle());
		hsExpr_free(m_haskellHandle);
		m_haskellHandle = updated;
	}
}

StmtCompound::~StmtCompound()
{
	qDeleteAll(m_statements);
}

QString StmtCompound::toString() const
{
	QStringList statementsStr;

	statementsStr.append(QString::number(m_statements.count()));

	foreach (const Stmt *s, m_statements)
		statementsStr.append(s->toString());

	return QString("StmtCompound(%1)").arg(statementsStr.join(", "));
}

StmtSetInsert::StmtSetInsert(const Idnt *setIdentifier, const Expr *elementToInsert)
: m_setIdentifier(setIdentifier), m_elementToInsert(elementToInsert)
{
	m_haskellHandle = hsStmtSetInsert_create(setIdentifier->haskellHandle(),
		elementToInsert->haskellHandle());
}

StmtSetInsert::~StmtSetInsert()
{
	delete m_setIdentifier;
	delete m_elementToInsert;
}

QString StmtSetInsert::toString() const
{
	return QString("StmtSetInsert(%1, %2)")
		.arg(m_setIdentifier->toString())
		.arg(m_elementToInsert->toString());
}

StmtAssignment::StmtAssignment(const Idnt *dest, const Expr *value)
: m_dest(dest), m_value(value)
{
	m_haskellHandle = hsStmtAssignment_create(dest->haskellHandle(),
		value->haskellHandle());
}

StmtAssignment::~StmtAssignment()
{
	delete m_dest;
	delete m_value;
}

QString StmtAssignment::toString() const
{
	return QString("StmtAssignment(%1, %2)")
		.arg(m_dest->toString())
		.arg(m_value->toString());
}

StmtIfElse::StmtIfElse(const Expr *cond, const Stmt *ifTrue, const Stmt *ifFalse)
: m_cond(cond), m_ifTrue(ifTrue), m_ifFalse(ifFalse)
{
	m_haskellHandle = hsStmtIfElse_create(cond->haskellHandle(),
		ifTrue->haskellHandle(), ifFalse->haskellHandle());
}

StmtIfElse::~StmtIfElse()
{
	delete m_cond;
	delete m_ifTrue;
	delete m_ifFalse;
}

QString StmtIfElse::toString() const
{
	return QString("StmtIfElse(%1, %2, %3)")
		.arg(m_cond->toString())
		.arg(m_ifTrue->toString())
		.arg(m_ifFalse->toString());
}

StmtChoiceOr::StmtChoiceOr(const Stmt *alt1, const Stmt *alt2)
: m_alt1(alt1), m_alt2(alt2)
{
	m_haskellHandle = hsStmtChoiceOr_create(alt1->haskellHandle(),
		alt2->haskellHandle());
}

StmtChoiceOr::~StmtChoiceOr()
{
	delete m_alt1;
	delete m_alt2;
}

QString StmtChoiceOr::toString() const
{
	return QString("StmtChoiceOr(%1, %2)")
		.arg(m_alt1->toString())
		.arg(m_alt2->toString());
}

StmtBranch::StmtBranch(const QString &targetNode)
: m_targetNode(targetNode)
{
	const QByteArray t = targetNode.toLatin1();
	m_haskellHandle = hsStmtBranch_create((void*)t.constData());
}

QString StmtBranch::toString() const
{
	return QString("StmtBranch(%1)").arg(m_targetNode);
}

}
}
}
