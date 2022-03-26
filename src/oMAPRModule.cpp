#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

#include "oMAPR.h"

// void finalizer_of_omapr( oMAPR* oMAPRptr ){
//   Rcpp::Rcout << "finalizer of oMAPR has been called\n";
//   omapR omap = oMAPRptr->omap;
//   // Rcpp::XPtr<omapR> ptr = oMAPRptr->ptr;
//   // ptr.release();
//   omap.clear();
//   delete oMAPRptr;
// }

void fff(oMAPR* ptr){
  Rcpp::Rcout << "finalizer of oMAPR has been called\n";
  //oMAPR oMAPRx = *ptr; //(oMAPR*)(R_ExternalPtrAddr(ptr));
  //Rcpp::XPtr<omapR> oMAPRptr = ptr->ptr;//s= Rcpp::XPtr<omapR>((omapR*)(R_ExternalPtrAddr(ptr->ptr)), true);
  ptr->omap.clear();
  Rcpp::Rcout << "xxx\n";
  //R_ClearExternalPtr(oMAPRptr);
  //delete ptr;
}
  

RCPP_MODULE(class_oMAPR) {
  using namespace Rcpp;

  class_<oMAPR>("oMAPR")

      .constructor<Rcpp::StringVector, Rcpp::List>()
      .constructor<Rcpp::XPtr<omapR>>()

      .field("ptr", &oMAPR::ptr)

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
      .method("values", &oMAPR::values)
      .finalizer(&fff);
}