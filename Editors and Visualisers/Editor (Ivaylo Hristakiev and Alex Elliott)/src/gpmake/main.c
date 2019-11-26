#include <stdio.h>

// Forward declarations of functions to appear
void showHelp();
void showOptimizationHelp();
void showWarningsHelp();

/*!
 * \brief Entry point for the GP build system
 * \param argc  The number of arguments passed to the program
 * \param argv  The argument vector
 * \return Integer, possible return values are documented in libGP
 */
int main(int argc, char **argv)
{
    if(argc < 2)
        showHelp();

    return 0;
}

/*!
 * \brief showHelp displays the application's basic help information
 */
void showHelp()
{
    printf("Usage: gpmake [ options ] project program\n");
    printf("\n");
    printf("Options:\n");
    printf("\t-h/--help\tDisplay this information\n");
    printf("\t--help={optimization|warnings}\n");
    printf("\t\t\tDetailed help on sub-topics\n");
    printf("\t--version\tDisplay version information\n");
    printf("\n");
    printf("Build Type Options:\n");
    printf("\t-a/--all\tBuild all output targets (executable and shared library)\n");
    printf("\t-e/--executable\tBuild an executable\n");
    printf("\t-s/--shared\tBuild a shared library\n");
    printf("\n");
    printf("Build Options:\n");
    printf("\t-v/--verbose\tDisplay debug information when compiling\n");
    printf("\t-o/--output=$FILE\n");
    printf("\t\t\tThe name of the output program to produce\n");
    printf("\t-d/--dest-dir=$DIR\n");
    printf("\t\t\tThe directory in which to place the completed output\n");
    printf("\t-b/--build-dir=$DIR\n");
    printf("\t\t\tThe directory in which build files should be placed\n");
    printf("\t-t/--tracing={true|false}\n");
    printf("\t\t\tDetermines whether the produced file supports tracing\n");
    printf("\t\t\tor not. Default value = false\n");
}
