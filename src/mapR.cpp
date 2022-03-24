#ifndef _MAPRHEADER_
#include "mapR.h"
#endif

omapR omapNew(Rcpp::StringVector keys, Rcpp::List values) {
  omapR omap;
  for(R_xlen_t i = 0; i < keys.size(); i++) {
    //SEXP v = values[i];
    omap.emplace(keys[i], values[i]);
  }
  return omap;
}

umapR umapNew(Rcpp::StringVector keys, Rcpp::List values) {
  umapR umap;
  for(R_xlen_t i = 0; i < keys.size(); i++) {
    //SEXP v = values[i];
    umap.emplace(keys[i], values[i]);
  }
  return umap;
}

std::vector<std::string> vectordiff(Rcpp::StringVector V, Rcpp::StringVector U){
  std::vector<std::string> diff(0);
  for(Rcpp::String s : V) {
    if(std::find(U.begin(), U.end(), s) == U.end()){
      diff.push_back(s);
    }
  }
  return diff;
}

// Rcpp::XPtr<mapR> mapPointer(std::vector<std::string> keys, Rcpp::List values)
// {
//   mapR map;
//   for(size_t i = 0; i < keys.size(); i++) {
//     std::vector<double> v = values[i];
//     map.emplace(keys[i], v);
//   }
//   Rcpp::XPtr<mapR> ptr(new mapR(map), true);
//   // ptr.attr("class") = "mapR";
//   return ptr;
// }



//#include "uMAPR.h"



// RCPP_MODULE(maprModule) {
//   using namespace Rcpp;
//   class_<MAPR>("MAPR")
//       .constructor<std::vector<std::string>, Rcpp::List>()
//       .field_readonly("ptr", &MAPR::ptr);
//   // .method("mapPointer", &MAPR::toMAPR);
// }

// RCPP_MODULE(umaprModule) {
//   using namespace Rcpp;
//   class_<uMAPR>("uMAPR")
//       .constructor<Rcpp::StringVector, Rcpp::List>()
//       .field_readonly("ptr", &uMAPR::ptr)
//       .method("size", &uMAPR::size)
//       .method("at", &uMAPR::at)
//       .method("has_key", &uMAPR::has_key)
//       .method("insert", &uMAPR::insert)
//       .method("assign", &uMAPR::assign)
//       .method("erase", &uMAPR::erase)
//       .method("merase", &uMAPR::merase)
//       .method("merge", &uMAPR::merge)
//       .method("keys", &uMAPR::keys)
//       .method("values", &uMAPR::values);
// }

// class MAPRPTR {
//   Rcpp::XPtr<mapR> mapPTR;
//   mapR map;
// 
//  public:
//   MAPRPTR(Rcpp::XPtr<mapR> mapPTR_) : mapPTR(mapPTR_), map(*mapPTR_) {}
// 
// 
//   // protected:
//   //  Rcpp::XPtr<mapR> mapPTR;
//   //  mapR map;
// };
// 
// RCPP_MODULE(maprptrModule) {
//   using namespace Rcpp;
//   class_<MAPRPTR>("MAPRPTR")
//       .constructor<Rcpp::XPtr<mapR>>()
//       .method("size", &MAPRPTR::size)
//       .method("at", &MAPRPTR::at)
//       .method("index", &MAPRPTR::index)
//       .method("has_key", &MAPRPTR::has_key)
//       .method("nth", &MAPRPTR::nth)
//       .method("insert", &MAPRPTR::insert)
//       .method("assign", &MAPRPTR::assign)
//       .method("erase", &MAPRPTR::erase)
//       .method("merase", &MAPRPTR::merase)
//       .method("merge", &MAPRPTR::merge)
//       .method("keys", &MAPRPTR::keys)
//       .method("values", &MAPRPTR::values);
// }
