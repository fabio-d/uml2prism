#include <QApplication>
#include <QLabel>

#include "Gui/MainWindow.h"

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	QStringList args = QApplication::arguments();
	Gui::MainWindow *mw = new Gui::MainWindow();
	if (args.count() >= 2)
		mw->loadFile(args[1]);
	mw->show();
	return app.exec();
}
