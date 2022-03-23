#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

#include "uMAPR.h"

RCPP_MODULE(class_uMAPR) {
  using namespace Rcpp;

  class_<uMAPR>("uMAPR")

      .constructor<Rcpp::StringVector, Rcpp::List>()
      .constructor<Rcpp::XPtr<umapR>>()

      .field_readonly("ptr", &uMAPR::ptr)

      .method("size", &uMAPR::size)
      .method("at", &uMAPR::at)
      .method("has_key", &uMAPR::has_key)
      .method("insert", &uMAPR::insert)
      .method("assign", &uMAPR::assign)
      .method("erase", &uMAPR::erase)
      .method("merase", &uMAPR::merase)
      .method("extract", &uMAPR::extract)
      .method("merge", &uMAPR::merge)
      .method("keys", &uMAPR::keys)
      .method("keys2", &uMAPR::keys2)
      .method("values", &uMAPR::values);
}