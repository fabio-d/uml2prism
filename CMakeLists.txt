cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
project(UMLEditor)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

find_package(Qt4 REQUIRED QtGui QtXml)
#find_package(Qt5 REQUIRED Widgets)

add_definitions(-std=c++11 -Wall)

add_executable(UMLEditor
	Core/Document.cpp
	Core/UMLDiagram.cpp
	Core/UMLElement.cpp
	Gui/GraphicsAuxiliaryItems.cpp
	Gui/MainWindow.cpp
	Gui/UMLDiagramView.cpp
	Gui/UMLElement.cpp
	Gui/UMLGraphicsScene.cpp
	Gui/UMLToolboxListWidget.cpp
	resources/resources.qrc
	main.cpp
)

target_link_libraries(UMLEditor Qt4::QtGui Qt4::QtXml)
#target_link_libraries(UMLEditor Qt5::Widgets)

install(TARGETS UMLEditor DESTINATION bin)
