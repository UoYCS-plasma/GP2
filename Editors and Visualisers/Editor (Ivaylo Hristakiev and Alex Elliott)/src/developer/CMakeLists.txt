#
# Build file for GP Developer
#
# Copyright 2013 Alex Elliott <alex@alex-elliott.co.uk>
#
# ------------------------------------------------------------------------------
#
# This file defines the rules necessary to create GP Developer, the GP2 system's
# new Integrated Development Environment
#

# Get all of the .cpp files via a glob pattern, we need to build all of them
FILE(GLOB_RECURSE GPDeveloper_CPP_SRCS src/developer/*.cpp)

# This is a Qt-based application, and therefore we need a list of header files
# which contain a QObject-derived class so that they can be passed through moc
# (the Qt meta-object compiler)
#
# To any unfamiliar with Qt this is what causes the build system to generate
# moc_${filename}.cxx files and then compile them.
SET(GPDeveloper_HPP_SRCS
    src/developer/graphview/edgeitem.hpp
    src/developer/graphview/editedgedialog.hpp
    src/developer/graphview/editnodedialog.hpp
    src/developer/graphview/graphitem.hpp
    src/developer/graphview/graphwidget.hpp
    src/developer/graphview/graphscene.hpp
    src/developer/graphview/nodeitem.hpp
    src/developer/preferences/appearancepreferences.hpp
    src/developer/preferences/preferencesdialog.hpp
    src/developer/preferences/preferencespage.hpp
    src/developer/preferences/projectpreferences.hpp
    src/developer/preferences/toolchainpreferences.hpp
    src/developer/aboutdialog.hpp
    src/developer/codeeditor.hpp
    src/developer/conditioneditor.hpp
    src/developer/conditionhighlighter.hpp
    src/developer/edge.hpp
    src/developer/edit.hpp
    src/developer/firstrundialog.hpp
    src/developer/gpfile.hpp
    src/developer/graph.hpp
    src/developer/graphedit.hpp
    src/developer/helpdialog.hpp
    src/developer/importgraphdialog.hpp
    src/developer/importprogramdialog.hpp
    src/developer/importruledialog.hpp
    src/developer/listvalidator.hpp
    src/developer/mainwindow.hpp
    src/developer/newgraphdialog.hpp
    src/developer/newprojectwizard.hpp
    src/developer/newprogramdialog.hpp
    src/developer/newruledialog.hpp
    src/developer/node.hpp
    src/developer/openprojectprogressdialog.hpp
    src/developer/prettytab.hpp
    src/developer/prettytabbar.hpp
    src/developer/prettytabheader.hpp
    src/developer/prettytabmenu.hpp
    src/developer/prettytabwidget.hpp
    src/developer/program.hpp
    src/developer/programedit.hpp
    src/developer/programeditor.hpp
    src/developer/programhighlighter.hpp
    src/developer/project.hpp
    src/developer/quickrunwidget.hpp
    src/developer/results.hpp
    src/developer/rule.hpp
    src/developer/ruleedit.hpp
    src/developer/run.hpp
    src/developer/runconfig.hpp
    src/developer/runconfiguration.hpp
    src/developer/styledbutton.hpp
    src/developer/welcome.hpp
)

# The UI files must be passed to uic as well to be compiled into headers
SET(GPDeveloper_UI_SRCS
    src/developer/graphview/editedgedialog.ui
    src/developer/graphview/editnodedialog.ui
    src/developer/preferences/appearancepreferences.ui
    src/developer/preferences/preferencesdialog.ui
    src/developer/preferences/projectpreferences.ui
    src/developer/preferences/toolchainpreferences.ui
    src/developer/aboutdialog.ui
    src/developer/edit.ui
    src/developer/firstrundialog.ui
    src/developer/graphedit.ui
    src/developer/importgraphdialog.ui
    src/developer/importprogramdialog.ui
    src/developer/importruledialog.ui
    src/developer/helpdialog.ui
    src/developer/mainwindow.ui
    src/developer/newgraphdialog.ui
    src/developer/newprojectwizard.ui
    src/developer/newprogramdialog.ui
    src/developer/newruledialog.ui
    src/developer/openprojectprogressdialog.ui
    src/developer/prettytabheader.ui
    src/developer/prettytabwidget.ui
    src/developer/programedit.ui
    src/developer/quickrunwidget.ui
    src/developer/results.ui
    src/developer/ruleedit.ui
    src/developer/run.ui
    src/developer/runconfiguration.ui
    src/developer/styledbutton.ui
    src/developer/welcome.ui
)

# Finally the QRC resource files must also be compiled into .cpp (or as CMake
# uses .cxx) files so that they can become objects in the final linking phase.
SET(GPDeveloper_QRC_SRCS
    src/developer/icons.qrc
    src/developer/images.qrc
    src/developer/stylesheets.qrc
    src/developer/templates.qrc
)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${GP2_SOURCE_DIR})


# We need to link against the Qt libraries located in the top-level
# CMakeLists.txt
if(WIN32)
SET(GPDeveloper_LINK_LIBS ${QT_LIBRARIES} ${OGDF_LIBRARY} ${libGP_NAME} libTranslate psapi)
ELSE()
SET(GPDeveloper_LINK_LIBS ${QT_LIBRARIES} ${OGDF_LIBRARY} ${libGP_NAME} libTranslate)
ENDIF()

# These lines tell the build system to perform the tasks mentioned above
QT4_ADD_RESOURCES(GPDeveloper_CPP_SRCS ${GPDeveloper_QRC_SRCS})
QT4_WRAP_UI(GPDeveloper_CPP_SRCS ${GPDeveloper_UI_SRCS})
QT4_WRAP_CPP(GPDeveloper_CPP_SRCS ${GPDeveloper_HPP_SRCS})

INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS} ${OGDF_INCLUDE_DIR})

# This block covers the actual creation of the gpdeveloper binary
IF(UNIX)
    ADD_EXECUTABLE(gpdeveloper ${GPDeveloper_CPP_SRCS})
ELSEIF(WIN32)
    ADD_EXECUTABLE(gpdeveloper WIN32 ${GPDeveloper_CPP_SRCS})
ENDIF()

# Which is then linked against the located libraries (such as Qt) to produce an
# executable which can actually run
TARGET_LINK_LIBRARIES(gpdeveloper ${GPDeveloper_LINK_LIBS})

SET(CMAKE_INSTALL_PREFIX .)

# Per-platform CPack Configuration
IF(UNIX AND NOT APPLE)
    SET(CPACK_GENERATOR TGZ)

    # Obtain the Qt shared libraries - HACK
    set (_resolvedQtObjects "")
    foreach (_file ${QT_LIBRARIES})
        get_filename_component(_resolvedQtObjects "${_file}" REALPATH)
        list (APPEND _resolvedQtObjects "${_resolvedFile}")
    endforeach()

    INSTALL(FILES  ${_resolvedQtObjects} ${OGDF_INCLUDE_DIR}/libOGDF.so
            DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )

    INSTALL(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/gpdeveloper DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
ELSEIF(APPLE)
    SET(CPACK_GENERATOR Bundle)
    INSTALL(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/gpdeveloper DESTINATION bin)
ELSEIF(WIN32)
    SET(CPACK_GENERATOR NSIS)
    SET(CPACK_NSIS_MUI_ICON ${GP2_SOURCE_DIR}/src/developer/images/graph.ico)

    # Get the Qt DLLs for packaging with this executable
    #INSTALL(PROGRAMS ${QT_BINARY_DIR}/QtCore4.dll
    #                 ${QT_BINARY_DIR}/QtGui4.dll
    #                 ${QT_BINARY_DIR}/mingwm10.dll
    #                 ${QT_BINARY_DIR}/libgcc_s_dw2-1.dll
    #        DESTINATION bin)

    INSTALL(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/gpdeveloper.exe DESTINATION bin)
ENDIF()

# Bring in the CMakeLists.txt for the developer test cases
# INCLUDE("src/developer/tests/CMakeLists.txt")
