#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

#include "oMAPR.h"

void finalizer_of_omapr( oMAPR* ptr ){
  Rcpp::Rcout << "finalizer of oMAPR has been called\n";
  if(ptr){
    Rcpp::Rcout << "if ptr\n";
    omapR* ptrptr = (ptr->ptr).get();
    if(ptrptr){
      Rcpp::Rcout << "if ptrptr\n";
      delete ptrptr;
    }
    delete ptr;
  }
}



RCPP_MODULE(class_oMAPR) {
  using namespace Rcpp;

  class_<oMAPR>("oMAPR")

      .constructor<Rcpp::StringVector, Rcpp::List>()
      .constructor<Rcpp::XPtr<omapR>>()

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