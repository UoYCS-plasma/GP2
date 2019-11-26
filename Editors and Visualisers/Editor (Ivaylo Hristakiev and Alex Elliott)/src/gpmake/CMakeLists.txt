#
# Build file for GP Developer
#
# Copyright 2013 Alex Elliott <alex@alex-elliott.co.uk>
#
# ------------------------------------------------------------------------------
#
# This file builds gpmake - the GP2 Build Tool
#

SET(gpmake_C_SRCS
    src/gpmake/main.c
)

#TODO: link with libGP

ADD_EXECUTABLE(gpmake ${gpmake_C_SRCS})
