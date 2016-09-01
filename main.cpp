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

#include <QApplication>
#include <QLabel>

#include "Core/Compiler/Bindings_stub.h"

#include "Gui/MainWindow.h"

extern "C" void __stginit_Bindings(void);

int main(int argc, char *argv[])
{
	hs_init(&argc, &argv);
	hs_add_root(__stginit_Bindings);

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
