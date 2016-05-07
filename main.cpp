#include <QApplication>
#include <QLabel>

#include "Core/Compiler/Bindings_stub.h"

#include "Gui/MainWindow.h"

extern "C" void __stginit_Bindings(void);

int main(int argc, char *argv[])
{
	hs_init(&argc, &argv);
	hs_add_root(__stginit_Bindings);

	hsHelloWorld();

	QApplication app(argc, argv);
	QStringList args = QApplication::arguments();
	Gui::MainWindow *mw = new Gui::MainWindow();
	if (args.count() >= 2)
		mw->loadFile(args[1]);
	mw->show();

	int res = app.exec();
	hs_exit();

	return res;
}
