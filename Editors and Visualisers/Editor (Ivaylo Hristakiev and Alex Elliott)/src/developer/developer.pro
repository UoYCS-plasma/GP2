#-------------------------------------------------
#
# Project created by QtCreator 2012-10-09T11:27:57
#
#-------------------------------------------------

QT += core gui xml widgets svg

TARGET = GPDeveloper
TEMPLATE = app

INCLUDEPATH += src/developer

HEADERS += \
    welcome.hpp \
    styledbutton.hpp \
    runconfiguration.hpp \
    run.hpp \
    rule.hpp \
    results.hpp \
    project.hpp \
    program.hpp \
    prettytabwidget.hpp \
    prettytabmenu.hpp \
    prettytabheader.hpp \
    prettytabbar.hpp \
    prettytab.hpp \
    node.hpp \
    newprojectwizard.hpp \
    mainwindow.hpp \
    helpdialog.hpp \
    graph.hpp \
    gpfile.hpp \
    global.hpp \
    edit.hpp \
    edge.hpp \
    aboutdialog.hpp \
    graphview/nodeitem.hpp \
    graphview/graphwidget.hpp \
    graphview/edgeitem.hpp \
    preferences/projectpreferences.hpp \
    preferences/preferencespage.hpp \
    preferences/preferencesdialog.hpp \
    quickrunwidget.hpp \
    ruleparser.hpp \
    parsertypes.hpp \
    graphparser.hpp \
    ruleedit.hpp \
    programedit.hpp \
    graphedit.hpp \
    newruledialog.hpp \
    newprogramdialog.hpp \
    newgraphdialog.hpp \
    programhighlighter.hpp \
    codeeditor.hpp \
    programtokens.hpp \
    programeditor.hpp \
    token.hpp \
    preferences/appearancepreferences.hpp \
    preferences/toolchainpreferences.hpp \
    graphview/graphscene.hpp \
    graphview/graphitem.hpp \
    graphview/editnodedialog.hpp \
    graphview/editedgedialog.hpp \
    dotparser.hpp \
    conditioneditor.hpp \
    conditiontokens.hpp \
    conditionhighlighter.hpp \
    importprogramdialog.hpp \
    importruledialog.hpp \
    importgraphdialog.hpp \
    openprojectprogressdialog.hpp \
    list.hpp \
    firstrundialog.hpp \
    listvalidator.hpp \
    runconfig.hpp

FORMS += \
    welcome.ui \
    styledbutton.ui \
    runconfiguration.ui \
    run.ui \
    results.ui \
    prettytabwidget.ui \
    prettytabheader.ui \
    newprojectwizard.ui \
    mainwindow.ui \
    helpdialog.ui \
    edit.ui \
    aboutdialog.ui \
    preferences/projectpreferences.ui \
    preferences/preferencesdialog.ui \
    quickrunwidget.ui \
    ruleedit.ui \
    programedit.ui \
    graphedit.ui \
    newruledialog.ui \
    newprogramdialog.ui \
    newgraphdialog.ui \
    preferences/appearancepreferences.ui \
    preferences/toolchainpreferences.ui \
    graphview/editnodedialog.ui \
    graphview/editedgedialog.ui \
    importprogramdialog.ui \
    importruledialog.ui \
    importgraphdialog.ui \
    openprojectprogressdialog.ui \
    firstrundialog.ui

RESOURCES += \
    icons.qrc \
    templates.qrc \
    stylesheets.qrc \
    images.qrc

SOURCES += \
    welcome.cpp \
    styledbutton.cpp \
    runconfiguration.cpp \
    run.cpp \
    rule.cpp \
    results.cpp \
    project.cpp \
    program.cpp \
    prettytabwidget.cpp \
    prettytabmenu.cpp \
    prettytabheader.cpp \
    prettytabbar.cpp \
    prettytab.cpp \
    node.cpp \
    newprojectwizard.cpp \
    mainwindow.cpp \
    main.cpp \
    helpdialog.cpp \
    graph.cpp \
    gpfile.cpp \
    global.cpp \
    edit.cpp \
    edge.cpp \
    aboutdialog.cpp \
    graphview/nodeitem.cpp \
    graphview/graphwidget.cpp \
    graphview/edgeitem.cpp \
    preferences/projectpreferences.cpp \
    preferences/preferencespage.cpp \
    preferences/preferencesdialog.cpp \
    quickrunwidget.cpp \
    ruleparser.cpp \
    parsertypes.cpp \
    graphparser.cpp \
    ruleedit.cpp \
    programedit.cpp \
    graphedit.cpp \
    newruledialog.cpp \
    newprogramdialog.cpp \
    newgraphdialog.cpp \
    programhighlighter.cpp \
    codeeditor.cpp \
    programeditor.cpp \
    preferences/appearancepreferences.cpp \
    preferences/toolchainpreferences.cpp \
    graphview/graphscene.cpp \
    graphview/graphitem.cpp \
    graphview/editnodedialog.cpp \
    graphview/editedgedialog.cpp \
    dotparser.cpp \
    conditioneditor.cpp \
    conditionhighlighter.cpp \
    importprogramdialog.cpp \
    importruledialog.cpp \
    importgraphdialog.cpp \
    openprojectprogressdialog.cpp \
    list.cpp \
    firstrundialog.cpp \
    listvalidator.cpp \
    runconfig.cpp

OTHER_FILES += \
    templates/newproject.gpp \
    CMakeLists.txt \
    stylesheets/welcome.css \
    stylesheets/tabwidget.css \
    stylesheets/runconfiguration.css \
    stylesheets/preferences.css \
    stylesheets/main.css \
    stylesheets/helpdialog.css \
    documentation/namespace_developer.dox \
    documentation/developer_main.dox \
    tests/CMakeLists.txt \
    templates/newrule_alternative.gpr \
    templates/newgraph_alternative.gpg \
    templates/example_program.gpx \
    templates/newprogram.gpx \
    templates/example_graph.gpg \
    templates/example_graph_rhs.gpg \
    templates/example_large_graph.gv

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libgp/release/ -llibgp -L"C:\Program Files (x86)\OGDF\lib" -llibOGDF -lpsapi
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libgp/debug/ -llibgp -L"C:\Program Files (x86)\OGDF\lib" -llibOGDF -lpsapi
else:unix: LIBS += -L$$OUT_PWD/../libgp/ -llibgp -lOGDF

win32: INCLUDEPATH += "C:\Program Files (x86)\OGDF\include"
win32: INCLUDEPATH += "C:\Users\Alex\Downloads\boost_1_53_0"

INCLUDEPATH += $$PWD/../libgp
INCLUDEPATH += $$PWD
DEPENDPATH += $$PWD/../libgp
