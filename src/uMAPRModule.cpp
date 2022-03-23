#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

umapR umapNew(Rcpp::StringVector keys, Rcpp::List values) {
  umapR umap;
  for(size_t i = 0; i < keys.size(); i++) {
    SEXP v = values[i];
    umap.emplace(keys[i], v);
  }
  return umap;
}

#include "uMAPR.h"

RCPP_MODULE(class_uMAPR) {

  using namespace Rcpp;

    class_<uMAPR>("uMAPR")

    .constructor<Rcpp::StringVector,Rcpp::List>()

    .field_readonly("ptr", &uMAPR::ptr)

    .method("size", &uMAPR::size)
    .method("at", &uMAPR::at)
    .method("has_key", &uMAPR::has_key)
    .method("insert", &uMAPR::insert)
    .method("assign", &uMAPR::assign)
    .method("erase", &uMAPR::erase)
    .method("merase", &uMAPR::merase)
    .method("merge", &uMAPR::merge)
    .method("keys", &uMAPR::keys)
    .method("values", &uMAPR::values)
    ;
}
