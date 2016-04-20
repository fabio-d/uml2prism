#include <QApplication>
#include <QLabel>

#include "Gui/MainWindow.h"

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	Gui::MainWindow mw;
	mw.show();
	return app.exec();
}
