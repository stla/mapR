#ifndef _MAPRHEADER_
#include "mapR_types.h"
#endif

#include "uMAPR.h"

void finalizer_of_umapr(uMAPR* ptr) {
  // Rcpp::Rcout << "finalizer of uMAPR has been called\n";
  // Rcpp::XPtr<umapR> ptrptr = ptr->ptr;
  // delete ptrptr.get();
  umapR umap = ptr->umap;
  umap.clear();
  //  delete ptr;
}

// [[Rcpp::export]]
Rcpp::RObject at3(Rcpp::XPtr<umapR> ptr, std::string key){
  umapR umap = *(ptr.get());
  return umap.at(key);
}

RCPP_MODULE(class_uMAPR) {
  using namespace Rcpp;

  class_<uMAPR>("uMAPR")

      .constructor<Rcpp::StringVector, Rcpp::List>()
      .constructor<Rcpp::XPtr<umapR>>()

      .field("ptr", &uMAPR::ptr)

      .method("size", &uMAPR::size)
      .method("at", &uMAPR::at)
      .method("at2", &uMAPR::at2)
      .method("has_key", &uMAPR::has_key)
      .method("insert", &uMAPR::insert)
      .method("assign", &uMAPR::assign)
      .method("erase", &uMAPR::erase)
      .method("merase", &uMAPR::merase)
      .method("extract", &uMAPR::extract)
      .method("extract_inplace", &uMAPR::extract_inplace)
      .method("extract_by_erasing", &uMAPR::extract_by_erasing)
      .method("extract_by_erasing_inplace", &uMAPR::extract_by_erasing_inplace)
      .method("merge", &uMAPR::merge)
      .method("keys", &uMAPR::keys)
      .method("values", &uMAPR::values)
      .method("toList", &uMAPR::toList)
      .finalizer(&finalizer_of_umapr);
}