#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

#include "oMAPR.h"

RCPP_MODULE(class_oMAPR) {
  using namespace Rcpp;

  class_<oMAPR>("oMAPR")

      .constructor<Rcpp::StringVector, Rcpp::List>()

      .field_readonly("ptr", &oMAPR::ptr)

      .method("size", &oMAPR::size)
      .method("at", &oMAPR::at)
      .method("has_key", &oMAPR::has_key)
      .method("index", &oMAPR::index)
      .method("nth", &oMAPR::nth)
      .method("insert", &oMAPR::insert)
      .method("assign", &oMAPR::assign)
      .method("erase", &oMAPR::erase)
      .method("merase", &oMAPR::merase)
      .method("merge", &oMAPR::merge)
      .method("keys", &oMAPR::keys)
      .method("values", &oMAPR::values);
}