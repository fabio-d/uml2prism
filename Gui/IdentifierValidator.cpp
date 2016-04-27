#include "Gui/IdentifierValidator.h"

#include <QMessageBox>

static const QRegExp regexp("^[_a-zA-Z0-9]*$");
static const QRegExp strict_regexp("^[_a-zA-Z][_a-zA-Z0-9]*$");

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
	return strict_regexp.exactMatch(str);
}

bool StrictIdentifierValidator::checkWithMessageBox(QWidget *parent, const QString &str)
{
	if (isAcceptable(str))
		return true;

	if (str.isEmpty())
		QMessageBox::warning(parent, "Invalid identifier", "Identifiers cannot be empty strings");
	else
		QMessageBox::warning(parent, "Invalid identifier", "Identifiers can only contain letters, digits or underscores and must not start with a digit");

	return false;
}

}
