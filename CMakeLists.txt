cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
set (CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_SOURCE_DIR}/cmake-haskell")
project(ModelEditor CXX Haskell)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(Qt4 REQUIRED QtGui QtXml QtSvg)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler)

# Ask ghc to compile a C file for us, then try to find out where HsFFI.h was
# included from
if (NOT CMAKE_Haskell_HsFFI_DIR)
	file (WRITE ${CMAKE_BINARY_DIR}/__testHsFFIPath/x.c "#include \"HsFFI.h\"\nint main() {}\n")
	execute_process(
		COMMAND ${CMAKE_Haskell_COMPILER} -optc-save-temps x.c -no-hs-main
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/__testHsFFIPath)
	execute_process(
		COMMAND sed -n "s/# [0-9]* \"\\(.*\\)HsFFI\\.h.*/\\1/p" x.i
		COMMAND head -n1
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/__testHsFFIPath
		OUTPUT_VARIABLE __output)
	file(TO_CMAKE_PATH "${__output}" CMAKE_Haskell_HsFFI_DIR)
endif ()
if (CMAKE_Haskell_HsFFI_DIR)
	message("-- Haskell compiler's HsFFI.h found in " ${CMAKE_Haskell_HsFFI_DIR})
else ()
	message(SEND_ERROR "Cannot locate Haskell compiler's HsFFI.h")
endif ()

add_definitions(-Wall)
include_directories(${CMAKE_Haskell_HsFFI_DIR})
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
set(CMAKE_Haskell_FLAGS "${CMAKE_Haskell_FLAGS} -stubdir \"${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler\"")
set(CMAKE_EXE_LINKER_FLAGS "-package uniplate -no-hs-main")

bison_target(CompilerParser Core/Compiler/Parser.yy
	${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler/Parser.cpp)
flex_target(CompilerLexer Core/Compiler/Lexer.l
	${CMAKE_CURRENT_BINARY_DIR}/Core/Compiler/Lexer.cpp)
add_flex_bison_dependency(CompilerLexer CompilerParser)

add_library(Haskell_CoreCompiler STATIC
	Core/Compiler/SemanticTree.hs
	Core/Compiler/PrismOutput.hs
	Core/Compiler/Compiler.hs
	Core/Compiler/Bindings.hs
)
set_property(TARGET Haskell_CoreCompiler PROPERTY LINKER_LANGUAGE Haskell)

add_executable(ModelEditor
	Core/DatatypeName.cpp
	Core/Document.cpp
	Core/ModelBuilder.cpp
	Core/PredicateList.cpp
	Core/UMLDiagram.cpp
	Core/UMLElement.cpp

	Core/Compiler/Compiler.cpp
	Core/Compiler/SemanticContext.cpp
	Core/Compiler/SemanticTree.cpp
	Core/Compiler/SemanticTreeGenerator.cpp
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
	Gui/EditPredicateDialog.cpp
	Gui/EditScriptedNodeElementDialog.cpp
	Gui/EditSignalEdgeDialog.cpp
	Gui/GraphicsAuxiliaryItems.cpp
	Gui/IdentifierValidator.cpp
	Gui/MainWindow.cpp
	Gui/ModelPreviewDialog.cpp
	Gui/ModelRunDialog.cpp
	Gui/PredicateListEditWidget.cpp
	Gui/RenameDialog.cpp
	Gui/UMLDiagramView.cpp
	Gui/UMLElement.cpp
	Gui/UMLGraphicsScene.cpp
	Gui/UMLToolboxListWidget.cpp
	Gui/UndoManager.cpp
	resources/resources.qrc
	main.cpp
)

set_property(TARGET ModelEditor PROPERTY LINKER_LANGUAGE Haskell)
target_link_libraries(ModelEditor Qt4::QtGui Qt4::QtXml Qt4::QtSvg Haskell_CoreCompiler)

install(TARGETS ModelEditor DESTINATION bin)
