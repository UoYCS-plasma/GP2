#
# Build file for GP Developer
#
# Copyright 2013 Alex Elliott <alex@alex-elliott.co.uk>
#
# ------------------------------------------------------------------------------
#
# This file builds gprc - the GP2 Rule Compiler
#

SET(gprc_C_SRCS
    src/gprc/main.c
)

#TODO: link with libGP

ADD_EXECUTABLE(gprc ${gprc_C_SRCS})
