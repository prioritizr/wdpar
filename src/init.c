#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _wdpar_rcpp_st_extract_holes(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_wdpar_rcpp_st_extract_holes", (DL_FUNC) &_wdpar_rcpp_st_extract_holes, 1},
    {NULL, NULL, 0}
};

void R_init_wdpar(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
