// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif


RcppExport SEXP _rcpp_module_boot_class_ErrorOrObject();
RcppExport SEXP _rcpp_module_boot_class_EitherFunc();
RcppExport SEXP _rcpp_module_boot_class_oMAPR();
RcppExport SEXP _rcpp_module_boot_class_uMAPR();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_class_ErrorOrObject", (DL_FUNC) &_rcpp_module_boot_class_ErrorOrObject, 0},
    {"_rcpp_module_boot_class_EitherFunc", (DL_FUNC) &_rcpp_module_boot_class_EitherFunc, 0},
    {"_rcpp_module_boot_class_oMAPR", (DL_FUNC) &_rcpp_module_boot_class_oMAPR, 0},
    {"_rcpp_module_boot_class_uMAPR", (DL_FUNC) &_rcpp_module_boot_class_uMAPR, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_mapR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
