TEMPLATE = lib
CONFIG -= qt
CONFIG += staticlib

SOURCES += \
    global.c \
    errors.c

HEADERS += \
    global.h \
    errors.h

OTHER_FILES += \
    CMakeLists.txt \
    documentation/libgp_main.dox

