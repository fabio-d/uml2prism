cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
project(UMLEditor)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(Qt4 REQUIRED QtGui QtXml QtSvg)

add_definitions(-std=c++11 -Wall)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler)
bison_target(CompilerParser Core/Compiler/Parser.yy
	${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler/Parser.cpp)
flex_target(CompilerLexer Core/Compiler/Lexer.l
	${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler/Lexer.cpp)
add_flex_bison_dependency(CompilerLexer CompilerParser)

add_executable(UMLEditor
	Core/DatatypeName.cpp
	Core/Document.cpp
	Core/ModelBuilder.cpp
	Core/UMLDiagram.cpp
	Core/UMLElement.cpp

	Core/Compiler/SemanticContext.cpp
	Core/Compiler/SourceLocation.cpp
	Core/Compiler/SyntaxTree.cpp
	Core/Compiler/SyntaxTreeGenerator.cpp
	${BISON_CompilerParser_OUTPUTS}
	${FLEX_CompilerLexer_OUTPUTS}

	Gui/EditClassDialog.cpp
	Gui/EditDatatypeNameWidget.cpp
	Gui/EditEnumerationDialog.cpp
	Gui/EditGlobalVariablesDialog.cpp
	Gui/EditListWidget.cpp
	Gui/EditScriptedNodeElementDialog.cpp
	Gui/EditSignalEdgeDialog.cpp
	Gui/GraphicsAuxiliaryItems.cpp
	Gui/IdentifierValidator.cpp
	Gui/MainWindow.cpp
	Gui/RenameDialog.cpp
	Gui/UMLDiagramView.cpp
	Gui/UMLElement.cpp
	Gui/UMLGraphicsScene.cpp
	Gui/UMLToolboxListWidget.cpp
	Gui/UndoManager.cpp
	resources/resources.qrc
	main.cpp
)

target_link_libraries(UMLEditor Qt4::QtGui Qt4::QtXml Qt4::QtSvg)

install(TARGETS UMLEditor DESTINATION bin)
