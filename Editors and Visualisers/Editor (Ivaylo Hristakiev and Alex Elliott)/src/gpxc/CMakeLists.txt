#
# Build file for GP Developer
#
# Copyright 2013 Alex Elliott <alex@alex-elliott.co.uk>
#
# ------------------------------------------------------------------------------
#
# This file builds gpxc - the GP2 Program Compiler
#

SET(gpxc_C_SRCS
    src/gpxc/main.c
)

#TODO: link with libGP

ADD_EXECUTABLE(gpxc ${gpxc_C_SRCS})
