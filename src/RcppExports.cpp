// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif


RcppExport SEXP _rcpp_module_boot_maprModule();
RcppExport SEXP _rcpp_module_boot_umaprModule();
RcppExport SEXP _rcpp_module_boot_maprptrModule();
RcppExport SEXP _rcpp_module_boot_umaprptrModule();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_maprModule", (DL_FUNC) &_rcpp_module_boot_maprModule, 0},
    {"_rcpp_module_boot_umaprModule", (DL_FUNC) &_rcpp_module_boot_umaprModule, 0},
    {"_rcpp_module_boot_maprptrModule", (DL_FUNC) &_rcpp_module_boot_maprptrModule, 0},
    {"_rcpp_module_boot_umaprptrModule", (DL_FUNC) &_rcpp_module_boot_umaprptrModule, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_mapR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
