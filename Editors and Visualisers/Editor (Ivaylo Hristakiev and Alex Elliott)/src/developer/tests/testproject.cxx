/*!
 * \file
 *
 * This file contains a proof-of-concept unit test to show how such a set of
 * tests could be added to the build system
 */
#include<iostream>
//#include "project.hpp"

/*!
 * \brief testPath tests the path() method in Project
 * \return Integer, non-zero on failure
 */
int testPath()
{
    //Developer::Project *project = new Developer::Project();

    //if(!project->path().isEmpty())
    //    return 1;

    return 0;
}

/*!
 * \brief Entry point for this test program, run the tests
 * \return Integer, non-zero on any failure
 */
int main(void)
{
    // Could also count failures with an integer counter, how this works is not
    // necessary to specify at this stage
    if(testPath() > 0)
        return 1;

    return 0;
}
