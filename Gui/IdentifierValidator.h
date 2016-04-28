#ifndef GUI_IDENTIFIERVALIDATOR_H
#define GUI_IDENTIFIERVALIDATOR_H

#include <QRegExpValidator>

namespace Gui
{

class IdentifierValidator : public QRegExpValidator
{
	public:
		IdentifierValidator(QObject *parent = nullptr);
};

class StrictIdentifierValidator : public QRegExpValidator
{
	public:
		StrictIdentifierValidator(QObject *parent = nullptr);

		static bool isAcceptable(const QString &str);
		static bool checkEnumValueWithMessageBox(QWidget *parent, const QString &str);
		static bool checkDatatypeWithMessageBox(QWidget *parent, const QString &str);
};

}

#endif // GUI_IDENTIFIERVALIDATOR_H
