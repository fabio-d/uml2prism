#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>

class Ui_MainWindow;

class MainWindow : public QMainWindow
{
	Q_OBJECT

	public:
		explicit MainWindow(QWidget *parent = nullptr);
		~MainWindow();

	private:
		Ui_MainWindow *ui;
};

#endif // GUI_MAINWINDOW_H
