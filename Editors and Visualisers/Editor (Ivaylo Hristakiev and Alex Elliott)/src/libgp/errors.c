#include "errors.h"

#ifdef __cplusplus
extern "C" {
#endif

const char *gp_ruleWarningString(int warningCode)
{
    const char *ret;
    switch(warningCode)
    {
        case GP_Rule_NoWarning:
            ret = "No warning returned";
            break;
        case GP_Rule_UnboundedInDegree:
            ret = "Unbounded in-degree, this program may run slowly";
            break;
        case GP_Rule_UnknownWarning:
        default:
            ret = "Unknown warning returned";
            break;
    }
    return ret;
}

const char *gp_ruleErrorString(int errorCode)
{
    const char *ret;
    switch(errorCode)
    {
        case GP_Rule_NoError:
            ret = "No error returned";
            break;
        case GP_Rule_InvalidLabel:
            ret = "Invalid label syntax. Labels must begin with an alphabetic character or an underscore followed by alphanumeric characters or underscores";
            break;
        case GP_Rule_UnknownError:
        default:
            ret = "Unknown error returned";
            break;
    }
    return ret;
}

const char *gp_programWarningString(int warningCode)
{
    const char *ret;
    switch(warningCode)
    {
        case GP_Program_NoWarning:
            ret = "No warning returned";
            break;
        case GP_Program_UnknownWarning:
        default:
            ret = "Unknown warning returned";
            break;

    }
    return ret;
}

const char *gp_programErrorString(int errorCode)
{
    const char *ret;
    switch(errorCode)
    {
        case GP_Program_NoError:
            ret = "No error returned";
            break;
        case GP_Program_SyntaxError:
            ret = "Invalid syntax detected in the provided program";
            break;
        case GP_Program_UnknownError:
        default:
            ret = "Unknown error returned";
            break;
    }
    return ret;
}

#ifdef __cplusplus
}
#endif
