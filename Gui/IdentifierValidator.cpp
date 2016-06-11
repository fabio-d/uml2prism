#include "Gui/IdentifierValidator.h"

#include <QMessageBox>

static const QRegExp regexp("^[_a-zA-Z0-9]*$");
static const QRegExp strict_regexp("^[_a-zA-Z][_a-zA-Z0-9]*$");

static QString notAllowedWordsMessage = "The following keywords are also forbidden: true, false, bool, nil, branch, if, else, choice, or, and names consisting of a single capital letter among A, E, F, G, R, U, W and X.";

namespace Gui
{

IdentifierValidator::IdentifierValidator(QObject *parent)
: QRegExpValidator(regexp, parent)
{
}

StrictIdentifierValidator::StrictIdentifierValidator(QObject *parent)
: QRegExpValidator(strict_regexp, parent)
{
}

bool StrictIdentifierValidator::isAcceptable(const QString &str)
{
	return strict_regexp.exactMatch(str)
		&& str != "true"
		&& str != "false"
		&& str != "bool"
		&& str != "nil"
		&& str != "branch"
		&& str != "if"
		&& str != "else"
		&& str != "choice"
		&& str != "or"
		&& str != "A"
		&& str != "E"
		&& str != "F"
		&& str != "G"
		&& str != "R"
		&& str != "U"
		&& str != "W"
		&& str != "X";
}

bool StrictIdentifierValidator::checkNodeNameWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid node name", "Node names cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid node name", "Node names can only contain letters, digits or underscores and must not start with a digit\n" + notAllowedWordsMessage);

	return false;
}

bool StrictIdentifierValidator::checkEnumValueWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid value", "Enumeration values cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid value", "Enumeration values can only contain letters, digits or underscores and must not start with a digit\n" + notAllowedWordsMessage);

	return false;
}

bool StrictIdentifierValidator::checkDatatypeWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid type name", "Type names cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid type name", "Type names can only contain letters, digits or underscores and must not start with a digit\n" + notAllowedWordsMessage);

	return false;
}

bool StrictIdentifierValidator::checkSignalNameWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid signal name", "Signal names cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid signal name", "Signal names can only contain letters, digits or underscores and must not start with a digit\n" + notAllowedWordsMessage);

	return false;
}

bool StrictIdentifierValidator::checkVariableNameWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid variable name", "Variable names cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid variable name", "Variable names can only contain letters, digits or underscores and must not start with a digit\n" + notAllowedWordsMessage);

	return false;
}

}
