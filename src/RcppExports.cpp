// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "mapR_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Left
Rcpp::XPtr<ErrorOrObject> Left(std::string x);
RcppExport SEXP _mapR_Left(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Left(x));
    return rcpp_result_gen;
END_RCPP
}
// Right
Rcpp::XPtr<ErrorOrObject> Right(Rcpp::RObject x);
RcppExport SEXP _mapR_Right(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Right(x));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_class_ErrorOrObject();
RcppExport SEXP _rcpp_module_boot_class_oMAPR();
RcppExport SEXP _rcpp_module_boot_class_uMAPR();

static const R_CallMethodDef CallEntries[] = {
    {"_mapR_Left", (DL_FUNC) &_mapR_Left, 1},
    {"_mapR_Right", (DL_FUNC) &_mapR_Right, 1},
    {"_rcpp_module_boot_class_ErrorOrObject", (DL_FUNC) &_rcpp_module_boot_class_ErrorOrObject, 0},
    {"_rcpp_module_boot_class_oMAPR", (DL_FUNC) &_rcpp_module_boot_class_oMAPR, 0},
    {"_rcpp_module_boot_class_uMAPR", (DL_FUNC) &_rcpp_module_boot_class_uMAPR, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_mapR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
