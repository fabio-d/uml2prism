cmake_minimum_required(VERSION 2.8.3)
project(UMLEditor)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

add_definitions(-std=c++11)

find_package(Qt5Widgets)

qt5_add_resources(qrcfiles
	resources.qrc
)

qt5_wrap_ui(uifiles
	Gui/MainWindow.ui
)

add_executable(UMLEditor
	Gui/MainWindow.cpp
	Gui/UMLGraphicsScene.cpp
	Gui/UMLToolboxListWidget.cpp
	main.cpp
	${qrcfiles} ${uifiles}
)

target_link_libraries(UMLEditor Qt5::Widgets)
install(TARGETS UMLEditor DESTINATION bin)
