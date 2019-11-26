/*!
 * \file
 */
#ifndef ERRORS_H
#define ERRORS_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
 * \brief The RuleWarnings enum contains placeholders for all of the types of
 *  warning rule compilation can throw
 */
enum GP_Rule_Warnings {
    //! Stub for value of 0, no issue
    GP_Rule_NoWarning,

    /*!
     * \brief This can mean poor performance
     *
     * \todo Elaborate on why
     */
    GP_Rule_UnboundedInDegree,

    //! Final catch-all upon failure
    GP_Rule_UnknownWarning
};

/*!
 * \brief Return a string describing the warning returned
 * \param warningCode   The warning code encountered, this should be a value
 *  from Rule_Warnings
 * \return A string describing the warning requested
 */
const char *gp_ruleWarningString(int warningCode);

enum GP_Rule_Errors {
    //! Stub for value of 0, no issue
    GP_Rule_NoError,

    //! Invalid label syntax
    GP_Rule_InvalidLabel,

    //! Final catch-all upon failure
    GP_Rule_UnknownError
};

const char *gp_ruleErrorString(int errorCode);

enum GP_Program_Warnings {
    //! Stub for value of 0, no issue
    GP_Program_NoWarning,

    //!

    //! Final catch-all upon failure
    GP_Program_UnknownWarning
};

const char *gp_programWarningString(int warningCode);

enum GP_Program_Errors {
    //! Stub for value of 0, no issue
    GP_Program_NoError,

    //! Syntax error in program
    GP_Program_SyntaxError,

    //! Final catch-all upon failure
    GP_Program_UnknownError
};

const char *gp_programErrorString(int errorCode);

#ifdef __cplusplus
}
#endif

#endif // ERRORS_H
