// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/container/flat_map.hpp>

typedef boost::container::flat_map<std::string, std::vector<double>> mapR;

Rcpp::XPtr<mapR> mapPointer(std::vector<std::string> keys, Rcpp::List values) {
  mapR map;
  for(size_t i=0; i<keys.size(); i++){
    std::vector<double> v = values[i];
    map.emplace(keys[i], v);
  }
  Rcpp::XPtr<mapR> ptr(new mapR(map), true);
  ptr.attr("class") = "mapR";
  return ptr;
}

class MAPR {
public:
  MAPR(std::vector<std::string> keys_, Rcpp::List values_) : keys(keys_), values(values_) {}
  
  Rcpp::XPtr<mapR> toMAPR() {
    return mapPointer(keys, values);
  }
  
private:
  std::vector<std::string> keys; Rcpp::List values;
};

RCPP_MODULE(maprModule) {
  using namespace Rcpp;
  class_<MAPR>("MAPR")
    .constructor<std::vector<std::string>, Rcpp::List>()
    .method("mapPointer", &MAPR::toMAPR);
}

class MAPRPTR {
public:
  MAPRPTR(Rcpp::XPtr<mapR> mapPTR_) : mapPTR(mapPTR_) {}
  
  std::vector<double> at(std::string key) {
    mapR map = *mapPTR;
    mapR::iterator it = map.find(key);
    if(it != map.end()){
      return it->second;
    }else{
      Rcpp::stop("Key not found.");
    }
  }
  
  std::vector<std::string> keys(){
    mapR map = *mapPTR;
    std::vector<std::string> out(0);
    for(mapR::iterator it = map.begin();it != map.end(); it++){
      out.push_back(it->first);
    }
    return out;
  }
  
  void insert(std::string key, std::vector<double> value){
    mapR map = *mapPTR;
    map.emplace(key, value);
    mapPTR = Rcpp::XPtr<mapR>(new mapR(map));
  }
  
protected:
  Rcpp::XPtr<mapR> mapPTR;
};

RCPP_MODULE(maprptrModule) {
  using namespace Rcpp;
  class_<MAPRPTR>("MAPRPTR")
    .constructor<Rcpp::XPtr<mapR>>()
    .method("at", &MAPRPTR::at)
    .method("insert", &MAPRPTR::insert)
    .method("keys", &MAPRPTR::keys);
}